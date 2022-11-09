{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ohua.Integration.Transform.DataPar where

-- import Ohua.Core.DFLang.PPrint (prettyExprM)

import qualified Ohua.Integration.Options as IOpt

import Data.Functor.Foldable (cata, embed)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Ohua.Backend.Lang as B
import Ohua.Core.ALang.Lang as AL
import Ohua.Core.ALang.Refs as ALRefs
import Ohua.Core.ALang.Util (destructure, findFreeBindings, findFreeVariables, lambdaArgsAndBody, substitute)
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.Lang as DFL hiding (length, substitute)
import qualified Ohua.Core.DFLang.Lang as DFL (length)
import qualified Ohua.Core.DFLang.Refs as DFRef
import Ohua.Core.Feature.TailRec.Passes.ALang as TR (y)
import Ohua.Core.Prelude hiding (concat, rewrite)

dataPar :: IOpt.Options -> CustomPasses
dataPar (IOpt.Options dpar amorph) =
  CustomPasses
  pure
  (case amorph of
     Just n -> amorphous n
     _ -> pure)
  ( case dpar of
      Just n -> liftPureFunctions
      _ -> pure)

invariantBroken :: Text -> OhuaM a
invariantBroken s = throwError $ "Invariant broken: " <> s

unsupported :: Text -> OhuaM a
unsupported s = throwError $ "Not supported: " <> s

-- |
-- This transformation wants to rewrite this:
-- @
--   for pair in pairs {
--     let x = before1(pair);
--     let z = before2(pair);
--     let path = find_path(mro, pair);
--     let y = after(path,z, v);
--     let r = maze.update(y);
--     rs.push(r);
--   }
-- @
-- into that:
-- @
--   let xs = Vec::new();
--   for pair in pairs {
--     let (x,z) = before(pair);
--     xs.push((x,z));
--   }
--   let paths = Vec::new();
--   for (x,z) in xs {
--     let path = find_path(mro, pair);
--     paths.push((path,z));
--   }
--   for (path,z) in paths {
--     let y = after(path,z,v);
--     let r = maze.update(y);
--     rs.push(r);
--   }
-- @
--
-- Performing the rewrite on ALang is not trivial because it requires
-- to capture results from `before` that are also used `after` the function
-- (in this case `find_path`) that were lifting out.
--
-- Our approach is to perform the same transformation on DFLang, so we get
-- around this problem.
-- Here is our challenge:
-- Let there be some function inside an SMap:
--                   +---+
--    smap --------->|   |   <result>
--    ctrl --------->| f | ------------>
--       g --------->|   |
--                   +---+
--
-- Then we need to perform the following steps:
--
-- 1. Clearly the result needs to be collected and smapped again:
--                   +---+
--    smap --------->|   |   <result>
--    ctrl --------->| f | ------------> collect --> smap
--       g --------->|   |
--                   +---+
-- Naturally, the ``collect`` gets the size from the `smap` that is located in.
-- For the smap that we are about to introduce, we can easily drop the size
-- channel because it just transports the same information as the initial `smap`.
-- *That is an invariant in our transformation!*
--
-- 2. Inject a `collect`-`smap` combo for every incoming arc.
--                                      +---+
--    smap ---- collect ---- smap ----->|   |   <result>
--    ctrl ---- collect ---- smap ----->| f | ------------> collect --> smap
--       g ---- collect ---- smap ----->|   |
--                                      +---+
-- Of course, the `size` for the `collect`s comes from the size channel of the
-- initial `smap`.
--
-- 3. Remove redundant `collect`-`smap` combos:
--                                      +---+
--    smap ---------------------------->|   |   <result>
--    ctrl ---- collect ---- smap ----->| f | ------------> collect --> smap
--       g ---- collect ---- smap ----->|   |
--                                      +---+
-- 4. Transform loop into parallelism.
-- Rewrite this:
--                                      +---+
--    smap ---------------------------->|   |   <result>
--    ctrl ---- collect ---- smap ----->| f | ------------> collect --> smap
--       g ---- collect ---- smap ----->|   |
--                                      +---+
-- into that:
--                                      +----------+
--    smap ---------------------------->| split-&- |  <futures>               <result>
--    ctrl ---- collect ---- smap ----->| spawn    | -----------> getFutures ---------> smap
--       g ---- collect ---- smap ----->|    <f>   |
--                                      +----------+

-- |
-- Ideally, SMap would be defined in DFLang as follows:
data SMap ty
  = SMap
      (DFApp 'Fun ty)
      -- ^ smapFun
      (NormalizedDFExpr ty)
      -- ^ body
      (DFApp 'Fun ty)
      -- ^ collect

spawnFuture :: QualifiedBinding
spawnFuture = "ohua.lang/spawnFuture"

joinFuture :: QualifiedBinding
joinFuture = "ohua.lang/collectFuture"

liftPureFunctions ::
  forall ty.
  NormalizedDFExpr ty ->
  OhuaM (NormalizedDFExpr ty)
liftPureFunctions = rewriteSMap
  where
    rewriteSMap :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteSMap (DFL.Let app cont) =
      case app of
        SMapFun{} ->
            let rewriteIt smap@(SMap _ smapBody _) = do
                  smap'@(SMap app' smapBody' coll') <- rewrite smap
                  case DFL.length smapBody of
                    -- TODO a stronger termination metric would be better
                    l | l /= DFL.length smapBody' -> rewriteIt smap'
                    _ -> pure $ \c -> DFL.Let app' $ appendExpr smapBody' $ DFL.Let coll' c
             in do
                  (smapBody, coll, cont') <- collectSMap cont
                  rewritten <- rewriteIt $ SMap app smapBody coll
                  rewritten <$> rewriteSMap cont'
        _ -> DFL.Let app <$> rewriteSMap cont
    rewriteSMap v = pure v

    -- collects the body of an smap for processing
    collectSMap ::
      NormalizedDFExpr ty ->
      OhuaM (NormalizedDFExpr ty, DFApp 'Fun ty, NormalizedDFExpr ty)
    collectSMap DFL.Var {} =
      invariantBroken
        "Found an smap expression not delimited by a collect"
    collectSMap (DFL.Let app cont) =
      case app of
        -- loop body has ended
        (PureDFFun _ (FunRef fn _ _) (_ :| [DFVar rty result]))
          | fn == DFRef.collect ->
            pure (DFL.Var (unwrapABnd result) rty, app, cont)
        SMapFun{} ->
            unsupported "Nested smap expressions"
        _ -> do
          (contBody, coll, cont') <- collectSMap cont
          return (DFL.Let app contBody, coll, cont')

rewrite :: forall ty. SMap ty -> OhuaM (SMap ty)
rewrite (SMap smapF body collectF) = do
  body' <- transformExprM rewriteBody body
  return $ SMap smapF body' collectF
  where
    rewriteBody :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteBody (DFL.Let fun@PureDFFun {} cont)
      | not (isIgnorable $ funRef fun) =
        liftFunction fun cont
    rewriteBody e = pure e

appendExpr ::
  NormalizedDFExpr ty ->
  NormalizedDFExpr ty ->
  NormalizedDFExpr ty
appendExpr (DFL.Let app cont) rest = DFL.Let app $ appendExpr cont rest
appendExpr DFL.Var {} rest = rest

isIgnorable :: QualifiedBinding -> Bool
isIgnorable (QualifiedBinding (NSRef ["ohua", "lang"]) _) = True
isIgnorable _ = False

liftFunction ::
  forall ty.
  DFApp 'Fun ty ->
  NormalizedDFExpr ty ->
  OhuaM (NormalizedDFExpr ty)
liftFunction (PureDFFun out fun inp) cont = do
  futuresBnd <- DataBinding <$> generateBindingWith "futures"
  outBound <- handleOutputSide futuresBnd
  let spawned = handleFun futuresBnd
  return $ DFL.Let spawned outBound
  where
    handleOutputSide :: ABinding 'Data -> OhuaM (NormalizedDFExpr ty)
    handleOutputSide futuresBnd =
      return $
      DFL.Let
        ( PureDFFun
            out
            (FunRef joinFuture Nothing (FunType $ Right $ TypeVar :| []))
            (DFVar TypeVar futuresBnd :| [])
        )
        cont

    handleFun :: ABinding 'Data -> DFApp 'Fun ty
    handleFun futuresBnd =
      PureDFFun
        (Direct futuresBnd)
        (FunRef spawnFuture Nothing (getNewFunType fun))
        (DFEnvVar TypeVar (FunRefLit fun) NE.<| inp)

    -- TODO the warning here is because there is no tight connection between the types and
    --      the terms
    getNewFunType :: FunRef ty -> FunType ty
    getNewFunType (FunRef _ _ (FunType (Left Unit))) = FunType $ Right $ TypeVar NE.:| []
    getNewFunType (FunRef _ _ (FunType (Right xs))) = FunType $ Right (TypeVar NE.<| xs)

-- TODO
-- DFLang transformations:
-- 1. create_runtime is just as a single node in the graph that gets executed once.
--    it dispatches its output to the spawn! (do we already fuse such things due to microservices???)
-- 2. don't collect any data! `spawn` wants to just issue a single call, so do it already!
--    there is absolutely no need to collect the data for the calls. `spawn` is just
--    a function in the loop!
-- 3. Same for collectWork!

-- |
-- All that the language and backend requires to support this transformations are
-- the implementations of the below functions.
lowerTaskPar :: lang -> arch -> B.TaskExpr ty -> B.TaskExpr ty
lowerTaskPar _ _ = cata $
  \case
    -- we need to do this on the Rust level because
    -- it would be hard to construct this call.
    e@(B.ApplyF (B.Stateless qb _args))
      | qb == spawnFuture -> embed e
    -- there is nothing to be done here.
    -- because we can implement this function easily in Rust.
    e@(B.ApplyF (B.Stateless qb _args))
      | qb == joinFuture -> embed e
    e -> embed e

takeN :: QualifiedBinding
takeN = "ohua.lang/takeN"

takeNLit = Lit $ FunRefLit $ FunRef takeN Nothing $ FunType $ Right $ TypeVar :| [TypeVar]

concat :: QualifiedBinding
concat = "ohua.lang/concat"

concatLit = Lit $ FunRefLit $ FunRef concat Nothing $ FunType $ Right $ TypeVar :| [TypeVar]

-- | This transformation adds a limit on the tries per round and therewith provides
--   a tuning knob to control the number of invalid (colliding) computation per round.
--
-- The transformation itself is jolly trivial. This
-- @
--   let f = \inputs ->
--     let results = smap (\input -> t) inputs in
--     let toBeRetried = detectCollisions results in
--     toBeRetried
--   in f inputs
-- @
-- transforms into that:
-- @
--  let (inputs',rest) = takeN n inputs in
--  let toBeRetried' = f inputs' in
--  let toBeRetried = concat toBeRetried' rest in ...
-- @
--
-- The challenge is to detect __when__ this optimization is to be applied.
-- Here are the ingredients:
--   * A recursion over a data structure (,e.g., a list)
--   * An smap (,i.e., a loop)
--   * a state as a result.
-- But their connection is the key:
-- @
--   let f = \state inputs ->
--             let someState = t_state in
--             let (inputs',[state']) = smap \input ->
--                                       let x = t_before input in
--                                       let (y,state') = state.g x in
--                                       let z = t_after y in
--                                       (z,state')
--                                     inputs in
--             let b = check inputs' in
--             if b
--             then f state' inputs' a
--             else state'
-- @
amorphous :: Integer -> AL.Expr ty -> OhuaM (AL.Expr ty)
amorphous numRetries targetExpr =
  transformM go targetExpr
  where
    -- TODO: Verify the correctness of the whole transformation
    go (Apply r@(PureFunction recurF Nothing) body)
      | recurF == TR.y = Apply r <$> rewriteIrregularApp body
    go e = pure e

    rewriteIrregularApp lam =
      let (ctxt, body) = lambdaArgsAndBody lam
       in case ctxt of
            ctxt' | length ctxt' < 2 ->
              pure lam
            ctxt' -> do
--              traceM $ "Found lambda body with sufficient length"
              result <- case findResult body of
                [r] ->
                  pure r
                _ ->
                  throwError $
                    "apparently, the recursion is not well-formed."
                      <> " invariant broken."
              let ctxtHS = HS.fromList ctxt'
              if HS.member result ctxtHS
                && isUsedState result body then (do
--                  traceM $ "result is in ctxt and used in body"
                case findLoops body of
                  [] -> pure lam
                  loops -> do
--                      traceM $ "Detected loops: " <> show loops
                    case filter ((`HS.member` ctxtHS) . snd3) loops of
                      [] -> pure lam
                      loops' -> do
--                          traceM $ "Result after filtering with ctxtHS: " <> show loops'
                        case filter (isUsedState result . fst3) loops' of
                          -- TODO this only checks for calls to the state but it should
                          --      really also check that the result of the loop is input to the same call
                          [] -> case filter (isUsedState result . third3) loops' of
                                  [] -> pure lam
                                  -- FIXME cleanup this code and merge it with the below version
                                  [(_, inp,_)] -> do
                                    recArgs <- findRecursionArgs body
                                    workResult <-
                                      findWorked body inp result recArgs
                                    rest <- generateBindingWith "rest"
--                                      traceM $ "state: " <> show result
--                                      traceM $ "wl: " <> show workResult
                                    case workResult of
                                      Just workResult' -> do
                                        lam' <- transformM (takeNRewrite' inp workResult' rest) lam
                                        transformM (concatRewrite result workResult' rest) lam'
                                      _ -> pure lam

                                  --FIXME I don't think we should error here but just not perform the transfomation.
                                  _ -> throwError $ "invariant broken: "
                                       <> "more than a single loop over the worklist found."
                          [(_, inp,_)] -> do
--                              traceM $ "Detected State usage: " <> show inp
                            recArgs <- findRecursionArgs body
                            workResult <-
                              findWorked body inp result recArgs
                            case workResult of
                              Just workResult' ->
                                transformM (doRewrite inp workResult') lam
                              _ -> pure lam
                          _ ->
                            throwError $
                              "invariant broken: "
                                <> "state inside a context can only be used once!") else pure lam

    findResult body =
      [ s | (AL.Let _ cond (AL.Var _)) <- universe body, ( Apply
                                                             (Apply (Apply (PureFunction ifTE Nothing) _) _)
                                                             ( AL.Lambda
                                                                 _
                                                                 (AL.Let _ (Apply (PureFunction idF Nothing) (AL.Var s)) _)
                                                               )
                                                           ) <-
                                                           universe cond, ifTE == ALRefs.ifThenElse && idF == ALRefs.id
      ]

    findRecursionArgs body =
      let calls =
            [ recursion
              | (AL.Let _ cond (AL.Var _)) <- universe body,
                (Apply (Apply (PureFunction ifTE Nothing) t) recursion) <- universe cond,
                ifTE == ALRefs.ifThenElse
            ]
       in case calls of
            [call] -> return $ findDerivations (HS.fromList $ findFreeBindings call) body
            _ -> throwError "invariant broken. recursion is not well-formed."

    findDerivations args expr =
      let args1 =
            [ b' | (AL.Let b (Apply (PureFunction _ _) (AL.Var b')) _) <- universe expr, HS.member b args
            ]
          args2 =
            [ b' | (AL.Let b (Apply (StatefulFunction _ _ (AL.Var b')) (Lit UnitLit)) _) <- universe expr, HS.member b args
            ]
       in case args1 ++ args2 of
            [] -> args
            args' -> HS.union args $ findDerivations (HS.fromList args') expr

    findWorked body inp result recArgs = do
      (b, smapBody,cont) <-
        case [ (b, smapBody,cont)
               | AL.Let
                   b
                   ( Apply
                       ( Apply
                           (PureFunction smapF Nothing)
                           smapBody
                         )
                       (AL.Var inp')
                     )
                   cont <-
                   universe body,
                 inp == inp'
             ] of
          [] -> throwError "invariant broken. I could not detext the already detected smap."
          [x] -> return x
          _ -> throwError "invariant broken. no ssa"
      case HS.member b recArgs of
        True -> return $ Just b -- functional case
        _ ->
          -- imperative case
          let fv = findFreeBindings smapBody
              fv' = filter (`isUsedState'` smapBody) fv
              fv'' = filter (/= result) fv'
          in case filter (`HS.member` recArgs) fv'' of
               [b'] -> return $ Just b'
               _ -> case fv'' of
                      [b'] -> case getWLPrime cont b' result of
                                [b''] | HS.member b'' recArgs -> return $ Just b'
                                _ -> return Nothing
                      bs ->
                        throwError $ "I was unable to detect the work variable: " <> show bs
    --return Nothing

    getWLPrime t wl result =
      [ v | (AL.Let
              v
             (Apply (StatefulFunction _ _ (AL.Var s)) (AL.Var d))
             cont) <- universe t
      , result == s && wl == d]

    isUsedState' bnd body =
      not $
        null
          [ s | (Apply (StatefulFunction _ _ (AL.Var s)) (AL.Var _)) <- universe body, s == bnd
          ]

    isUsedState bnd body =
      not $
        null
          [ s | BindState (AL.Var s) _ <- universe body, s == bnd
          ]

    findLoops body =
      [ (smapBody, inp, cont)
        | (AL.Let _ (Apply (Apply (PureFunction smapF Nothing) smapBody) (AL.Var inp)) cont) <- universe body,
          smapF == ALRefs.smap
      ]

    doRewrite
      inp
      worked
      ( AL.Let
          v
          (Apply f@(Apply (PureFunction smapF Nothing) _) (AL.Var inp'))
          cont
        )
        | smapF == ALRefs.smap && inp == inp' = do
          taken <- generateBindingWith "n_taken"
          takenInp <- generateBindingWith $ inp <> "_n"
          rest <- generateBindingWith "rest"
          nResults <- generateBindingWith "n_results"
          pendingWork <- generateBindingWith worked
          return $
            AL.Let
              taken
              ( Apply
                  (Apply takeNLit $ AL.Var inp)
                  $ Lit $ NumericLit numRetries
              )
              $ destructure (AL.Var taken) [takenInp, rest] $

                -- FIXME this assumes we take the result of an smap. but for an imperative case
                --       it is some state altered inside this smap!
                AL.Let v (Apply f $ AL.Var takenInp) $
                  AL.Let
                    pendingWork
                    (Apply (Apply concatLit $ AL.Var worked) $ AL.Var rest)
                    (substitute worked (AL.Var pendingWork) cont)
    doRewrite _ _ e = pure e

    takeNRewrite'
      inp
      worked
      rest
      ( AL.Let
          v
          (Apply f@(Apply (PureFunction smapF Nothing) _) (AL.Var inp'))
          cont
        )
        | smapF == ALRefs.smap && inp == inp' = do
          taken <- generateBindingWith "n_taken"
          takenInp <- generateBindingWith $ inp <> "_n"
          nResults <- generateBindingWith "n_results"
          return $
            AL.Let
              taken
              ( Apply
                  (Apply takeNLit $ AL.Var inp)
                  $ Lit $ NumericLit 10
              )
              $ destructure (AL.Var taken) [takenInp, rest] $ -- TODO take the value from the specification of lang (maybe via a type class)
                AL.Let v (Apply f $ AL.Var takenInp) cont
    takeNRewrite' _ _ _ e = pure e

    concatRewriteOn worked rest cont = do
      pendingWork <- generateBindingWith worked
      return $
        AL.Let pendingWork
        (Apply (Apply concatLit $ AL.Var worked) $ AL.Var rest)
        (substitute worked (AL.Var pendingWork) cont)


    concatRewrite state worked rest
      (AL.Let
          v
          f@(Apply (StatefulFunction _ _ (AL.Var s)) (AL.Var d))
          cont)
      | state == s && worked == d
      = do
          cont' <- concatRewriteOn v rest cont
          return $ AL.Let v f cont'
    concatRewrite _ _ _ e = pure e

fst3 (a,_,_) = a
snd3 (_,b,_) = b
third3 (_,_,c) = c
