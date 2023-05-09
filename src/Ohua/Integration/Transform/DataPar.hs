{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ohua.Integration.Transform.DataPar where

-- import Ohua.Core.DFLang.PPrint (prettyExprM)

import qualified Ohua.Integration.Options as IOpt

import Data.Functor.Foldable (cata, embed, cataA)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Ohua.Backend.Lang as B
import Ohua.Backend.Types as BT (FullTask(..), Architecture, Lang, Type)
import Ohua.Core.ALang.Lang as AL
import Ohua.Core.ALang.Refs as ALRefs
import Ohua.Core.ALang.Util (destructure, findFreeBindings, findFreeVariables, lambdaArgsAndBody, substitute)
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.Lang as DFL hiding (length, substitute)
import qualified Ohua.Core.DFLang.Lang as DFL (length)
import qualified Ohua.Core.DFLang.Refs as DFRef
import Ohua.Core.Feature.TailRec.Passes.ALang as TR (y)
import Ohua.Core.Prelude hiding (concat, rewrite)


dataPar :: forall ty. IOpt.Options -> (ty -> ty) -> CustomPasses ty
dataPar (IOpt.Options dpar amorph) dparLift =
  CustomPasses
  pure
  (case amorph of
     Just n -> amorphous n
     _ -> pure)
  ( case dpar of
      Just _n -> liftPureFunctions dparLift >=> typeAmorphous
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

liftPureFunctions :: forall ty.
                     (ty -> ty)
                  -> NormalizedDFExpr ty 
                  -> OhuaM (NormalizedDFExpr ty)
liftPureFunctions liftCollectTy = rewriteSMap
  where
    rewriteSMap :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteSMap (DFL.Let app cont) =
      case app of
        SMapFun{} ->
            let rewriteIt smap@(SMap _ smapBody _) = do
                  smap'@(SMap app' smapBody' coll') <- rewrite liftCollectTy smap
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
        (PureDFFun _ (FunRef fn _ _) (_ :| [DFVar atb@(DataBinding (TBind result rty))]))
          | fn == DFRef.collect ->
            pure (DFL.Var atb, app, cont)
            -- I don't know if this should happen and if we should learn anything from the parameter being a state here but
            -- I'll have the case in case we need it

        SMapFun{} ->
            unsupported "Nested smap expressions"
        _ -> do
          (contBody, coll, cont') <- collectSMap cont
          return (DFL.Let app contBody, coll, cont')

rewrite :: forall ty. (ty -> ty) -> SMap ty -> OhuaM (SMap ty)
rewrite liftCollectTy (SMap smapF body collectF) = do
  body' <- transformExprM rewriteBody body
  return $ SMap smapF body' collectF
  where
    rewriteBody :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteBody (DFL.Let fun@PureDFFun {} cont)
      | not (isIgnorable $ funRef fun) = do
        outTy <- findOutTy fun cont
        let outTy' = liftOutTy outTy
        liftFunction outTy' fun cont
    rewriteBody e = pure e
    
    -- Question: Can we get rid of this having the type annotation of the bound variable
    findOutTy :: DFApp 'Fun ty -> NormalizedDFExpr ty -> OhuaM (ArgType ty)
    findOutTy fun cont = 
        case DFL.outsDFApp fun of
            [TBind bnd ty] -> case findOutTy' bnd cont of
                        [] -> invariantBroken $ "found unused output:" <> show bnd <> " for function: " <> show fun
                        (t:_) -> return t
            _ -> unsupported "Multiple outputs to pure fun."
    
    findOutTy' :: Binding -> NormalizedDFExpr ty -> [ArgType ty]
    findOutTy' bnd cont = [ ty | DFL.Let app c <- universe' cont
                              , (TBind b ty) <- insAndTypesDFApp app
                              , b == bnd] 

    liftOutTy (Type t) = Type $ liftCollectTy t
    liftOutTy t = t

appendExpr ::
  NormalizedDFExpr ty ->
  NormalizedDFExpr ty ->
  NormalizedDFExpr ty
appendExpr (DFL.Let app cont) rest = DFL.Let app $ appendExpr cont rest
appendExpr DFL.Var {} rest = rest

isIgnorable :: QualifiedBinding -> Bool
isIgnorable (QualifiedBinding (NSRef ["ohua", "lang"]) _) = True
isIgnorable _ = False

liftFunction :: forall ty.
                ArgType ty
             -> DFApp 'Fun ty
             -> NormalizedDFExpr ty 
             -> OhuaM (NormalizedDFExpr ty)
liftFunction collectTy (PureDFFun out fun inp) cont = do
  -- Question: Whats the type of futureBnd supposed to be?
  futuresATBnd <- DataBinding . flip TBind TypeVar <$> generateBindingWith "futures"
  outBound <- handleOutputSide futuresATBnd
  let spawned = handleFun futuresATBnd
  return $ DFL.Let spawned outBound
  where
    handleOutputSide :: ATypedBinding 'Data ty -> OhuaM (NormalizedDFExpr ty)
    handleOutputSide futuresATBnd =
      return $
      DFL.Let
        ( PureDFFun
            out
            (FunRef joinFuture Nothing (FunType $ Right $ collectTy :| []))
            (DFVar futuresATBnd :| [])
        )
        cont

    handleFun :: ATypedBinding 'Data ty -> DFApp 'Fun ty
    handleFun futuresBnd =
      PureDFFun
        (Direct futuresBnd)
        (FunRef spawnFuture Nothing $ getSpawnFunType fun)
        (DFEnvVar TypeVar (FunRefLit fun) NE.<| inp)

    getSpawnFunType (FunRef _ _ (FunType (Left Unit))) = FunType $ Right $ TypeVar NE.:| []
    getSpawnFunType (FunRef _ _ (FunType (Right xs))) = FunType $ Right (TypeVar NE.<| xs)

-- |
-- All that the language and backend requires to support this transformations are
-- the implementations of the below functions.
lowerTaskPar :: forall lang arch ty. (ty ~ BT.Type (BT.Lang arch))
             => lang -> arch -> FullTask ty (B.TaskExpr ty) -> B.TaskExpr ty
lowerTaskPar _ arch = go 
    where
        go (FullTask _sends _recvs taskE) = 
            case  go' taskE of
                (taskE', _) -> taskE'

        -- This implementation does not need this state return anymore.
        -- I leave it in nevertheless to show how to work with cataA.
        go'  = flip runState False . cataA go''
        go'' :: B.TaskExprF ty (State Bool (B.TaskExpr ty)) -> (State Bool (B.TaskExpr ty))
        go'' = \case
                -- we need to do this on the Rust level because
                -- it would be hard to construct this call.
                e@(B.ApplyF (B.Stateless qb _args)) | qb == spawnFuture -> embed <$> sequence e
               
                -- there is nothing to be done here.
                -- because we can implement this function easily in Rust.
                e@(B.ApplyF (B.Stateless qb _args)) | qb == joinFuture -> do { put True; embed <$> sequence e }
                e -> embed <$> sequence e

takeN :: QualifiedBinding
takeN = "ohua.lang/takeN"

takeNLit ty = Lit $ FunRefLit $ FunRef takeN Nothing $ FunType $ Right $ ty :| [TypeVar]

concat :: QualifiedBinding
concat = "ohua.lang/concat"

concatLit ty = Lit $ FunRefLit $ FunRef concat Nothing $ FunType $ Right $ ty :| [ty]

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
amorphous numRetries = transformM go
  where
    -- TODO: Verify the correctness of the whole transformation
    go (Apply r@(PureFunction recurF Nothing) body)
      | recurF == TR.y = Apply r <$> rewriteIrregularApp body
    go e = pure e

    rewriteIrregularApp lam =
      let (ctxt, body) = lambdaArgsAndBody lam
       in case ctxt of
            ctxt' | length ctxt' < 2 -> pure lam
            ctxt' -> do
              -- traceM $ "Recursion lambda args: " <> show ctxt'
              stateResult <- case findResult body of
                               [r] -> pure r
                               _   -> throwError $ "apparently, the recursion is not well-formed." <> " invariant broken."
              recCallArgs <- findRecursionCallArgs body
              mStateIn <- findInParameters stateResult recCallArgs ctxt
              case mStateIn of
                Nothing -> do
                  -- traceM $ "Could not detect state input for: " <> show stateResult
                  pure lam
                Just stateIn -> do
                  -- traceM $ "looking for: " <> show stateIn
                  -- traceM $ "in context: " <> show ctxt
                  -- traceM $ "in body: " <> quickRender body
                  let ctxtHS = HS.fromList ctxt'
                  if HS.member stateIn ctxtHS
                    && isUsedStateAnywhere stateIn body
                  then (do
                           -- traceM $ "result is in ctxt and used in body"
                           case findLoops body of
                             [] -> pure lam
                             loops -> do
                               -- traceM $ "Detected loops: " <> show loops
                               -- refine to loops that use ctxt variables as their input.
                               case filter ((`HS.member` ctxtHS) . snd3) loops of
                                 [] -> pure lam
                                 loops' -> do
                                   -- traceM $ "Result after filtering with ctxtHS: " <> show loops'
                                   -- traceM $ "state: " <> show stateResult
                                   -- The state actually does not have to be updated inside the loop.
                                   -- We just have to perform the below code for all gathered loops.
                                   -- That way, we even support multiple worklists!
                                   let amorph lam' (loopBody, loopInp, _) =
                                         do
                                           -- traceM $ "Detected State usage in loop over: " <> show loopInp
                                           --recCallArgs <- findRecursionCallArgs body
                                           -- traceM $ "Recursion call args':" <> show recCallArgs
                                           wlRec <- case map snd $ filter (\(p,a) -> p == loopInp) $ zip ctxt recCallArgs of
                                                      [i] -> pure i
                                                      _ -> throwError "invariant broken" -- we detected that already above!
                                           let loopResults = findFreeStateVars loopBody
                                           case HS.member wlRec $ HS.fromList loopResults of
                                             -- the loop is the creator. concat right after it.
                                             True -> transformM (rewriteAfterLoop loopInp wlRec) lam'
                                             -- concat to the creator of the new worklist
                                             False -> do
                                               -- Question: What's the type of reest supposed to be?
                                               rest <- flip TBind TypeVar <$> generateBindingWith "rest"
                                               lam'' <- transformM (takeNRewrite loopInp rest) lam'
                                               transformM (concatRewrite wlRec rest) lam''
                                   foldM amorph lam loops'
                       )
                  else pure lam

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

    findInParameters stateResult recCallArgs ctxt =
      let idx = catMaybes $ map
                  (\(v,i) -> if v == stateResult then Just i else Nothing )
                  $ zip recCallArgs [0..]
      in case idx of
           [] -> pure Nothing -- result is not part of the recursion
           (x:[]) -> do
             let inVars = catMaybes $ map
                            (\(v,i) -> if i == x then Just v else Nothing)
                            $ zip ctxt [0..]
             case inVars of
                [s] -> pure $ Just s
                _ -> throwError "Invariant broken: recursion call args and recursion parameters do not have the same length."
           _ -> do
--             logInfoN $ "You passing the following parameter twice into the recursion: "
--                      <> show x
--                      <> " \nDo you really have to do that? (Aborting amorphous transaformation.) ".
             pure Nothing

    findRecursionCallArgs body =
      let calls =
            [ recursion
              | (AL.Let _ cond (AL.Var _)) <- universe body,
                (Apply (Apply (PureFunction ifTE Nothing) t) recursion) <- universe cond,
                ifTE == ALRefs.ifThenElse
            ]
      in case calls of
            [AL.Lambda _ (AL.Let _ recCall _)] -> return $ gatherArgs recCall
            _ -> throwError "invariant broken. recursion is not well-formed."

    gatherArgs (Apply a (AL.Var b)) = gatherArgs a ++ [b]
    gatherArgs _ = []

    findFreeStateVars :: AL.Expr ty -> [TypedBinding ty]
    findFreeStateVars e =
      let fv = findFreeBindings e
          fv' = filter (`isUsedState'` e) fv
      in fv'

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

    isUsedStateAnywhere bnd body =
      not $
        null
          [ s | (AL.Var s) <- universe body, s == bnd
          ]

    findLoops body =
      [ (smapBody, inp, cont)
        | (AL.Let _ (Apply (Apply (PureFunction smapF Nothing) smapBody) (AL.Var inp)) cont) <- universe body,
          smapF == ALRefs.smap
      ]

    rewriteAfterLoop
      loopIn@(TBind loopInBnd liTy)
      w@(TBind wl' wty)
      ( AL.Let
          v
          (Apply f@(Apply
                    (PureFunctionTy smapF _ (FunType (Right (inpTy :| _))) )
                    _)
                 (AL.Var loopInp'))
          cont
        )
        -- Question: What types are thos varaibles supposed to have?
        | smapF == ALRefs.smap && loopIn == loopInp' = do
          taken <- flip TBind TypeVar <$> generateBindingWith "n_taken"
          takenInp <- flip TBind liTy <$> generateBindingWith (loopInBnd <> "_n")
          rest <- flip TBind TypeVar <$> generateBindingWith "rest"
          nResults <- DataBinding . flip TBind TypeVar <$> generateBindingWith "n_results"
          pendingWork <- flip TBind TypeVar <$> generateBindingWith wl'
          return $
            AL.Let
              taken
              ( Apply
                  (Apply (takeNLit inpTy) $ AL.Var loopIn)
                  $ Lit $ NumericLit numRetries
              )
              $ destructure (AL.Var taken) [takenInp, rest] $

                AL.Let v (Apply f $ AL.Var takenInp) $
                  AL.Let
                    pendingWork
                    (Apply (Apply (concatLit inpTy) $ AL.Var w ) $ AL.Var rest)
                    (substitute wl' (AL.Var pendingWork) cont)
    rewriteAfterLoop _ _ e = pure e

    takeNRewrite
      loopIn@(TBind loopInBnd liTy)
      rest
      ( AL.Let
          v
           (Apply f@(Apply
                    (PureFunctionTy smapF _ (FunType (Right (inpTy :| _))))
                    _)
                 (AL.Var loopInp'))
          cont
        )
        | smapF == ALRefs.smap && loopIn == loopInp' = do
          -- Question: What are the types of varaibles supposed to be?
          taken <- flip TBind TypeVar <$> generateBindingWith "n_taken"
          takenInp <- flip TBind liTy <$> generateBindingWith (loopInBnd <> "_n")
          nResults <- flip TBind TypeVar <$> generateBindingWith "n_results"
          return $
            AL.Let
              taken
              ( Apply
                  (Apply (takeNLit inpTy) $ AL.Var loopIn)
                  $ Lit $ NumericLit numRetries
              )
              $ destructure (AL.Var taken) [takenInp, rest] $ -- TODO take the value from the specification of lang (maybe via a type class)
                AL.Let v (Apply f $ AL.Var takenInp) cont
    takeNRewrite _ _ e = pure e

    concatRewrite wl' rest
      (AL.Let
          v
          f -- @(Apply (StatefulFunctionTy _ _ (FunType (Right (_:| (inpTy:_)))) (AL.Var s)) (AL.Var d))
          cont)
      | wl' == v
      = do
          cont' <- concatRewriteOn v {- inpTy --} rest cont
          return $ AL.Let v f cont'
    concatRewrite _ _ e = pure e

    concatRewriteOn worked@(TBind workedB workedTy) {- inpTy -} rest cont = do
      pendingWork <- flip TBind workedTy <$> generateBindingWith workedB
      return $
        AL.Let pendingWork
        (Apply (Apply (concatLit {- inpTy -} TypeVar) $ AL.Var worked) $ AL.Var rest)
        (substitute workedB (AL.Var pendingWork) cont)

fst3   (a,_,_) = a
snd3   (_,b,_) = b
third3 (_,_,c) = c

typeAmorphous :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
typeAmorphous = return . mapFuns go
    where
        go (PureDFFun outs (FunRef f id (FunType (Right (a:|(b:c)) )) ) ins) | f == concat =
            PureDFFun outs (FunRef f id (FunType (Right (TypeList TypeVar :|(TypeList TypeVar:c)) )) ) ins
        go (PureDFFun outs (FunRef f id (FunType (Right (a :| b) )) ) ins) | f == takeN =
            PureDFFun outs (FunRef f id (FunType (Right (TypeList TypeVar :| b) )) ) ins
        go a = a
