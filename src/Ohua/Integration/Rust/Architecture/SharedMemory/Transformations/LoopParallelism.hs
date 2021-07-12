{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transformations.LoopParallelism where

import Ohua.Core.Prelude hiding (rewrite)
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.Lang
import qualified Ohua.Core.DFLang.Refs as DFRef

import Ohua.Types.Reference

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS

dataPar :: CustomPasses
dataPar = CustomPasses pure pure liftPureFunctions

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
--    ctrl ---------------------------->| f | ------------> collect --> smap
--       g ---- collect ---- smap ----->|   |
--                                      +---+
-- 4. Transform loop into parallelism.
-- Rewrite this:
--                                                   +---+
--    -- <xs> ---> smap ---------------------------->|   |   <result>
--    -- <arg> --> ctrl ---------------------------->| f | ------------> collect --> smap
--                    g ---- collect ---- smap ----->|   |
--                                                   +---+
-- into that:
--                                +----------+
--    -- <xs> ------------------->| split-&- |   <futures>               <result>
--    -- <arg> ------------------>| spawn    | ------------> getFutures ----------> smap
--           g ---- collect ----->|    <f>   |
--                                +----------+
--

-- |
-- Ideally, SMap would be defined in DFLang as follows:
data SMap ty =
  SMap
  -- | smapFun
  (DFApp 'Fun ty)
  -- | body
  (NormalizedDFExpr ty)
  -- | collect
  (DFApp 'Fun ty)

liftPureFunctions :: forall ty.
                     NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
liftPureFunctions = rewriteSMap
  where
    rewriteSMap :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteSMap (Let app cont) =
      case app of
        -- catch smapFun
        (PureDFFun _ fn _)
          | fn == DFRef.smapFun -> do
              (smapBody, coll, cont') <- collectSMap cont
              let smap = SMap app smapBody coll
              (SMap app' smapBody' coll') <- rewrite smap
              let e' = Let app' $ appendExpr smapBody' $ Let coll' cont'
              rewriteSMap e'
        _ -> Let app <$> rewriteSMap cont
    rewriteSMap v = pure v

    -- collects the body of a smap for processing
    collectSMap :: NormalizedDFExpr ty
                -> OhuaM (NormalizedDFExpr ty, DFApp 'Fun ty, NormalizedDFExpr ty)
    collectSMap Var{} =
      invariantBroken
      "Found an smap expression not delimited by a collect"
    collectSMap (Let app cont) =
      case app of
        -- loop body has ended
        (PureDFFun _ fn ((DFVar _ result) :|_))
          | fn == DFRef.collect ->
            pure (Let app $ Var $ unwrapABnd result, app, cont)
        (PureDFFun _ fn _)
          | fn == DFRef.smapFun ->
              unsupported "Nested smap expressions"
        _ -> do (contBody, coll, cont') <- collectSMap cont
                return (Let app contBody, coll, cont')

rewrite :: forall ty.SMap ty -> OhuaM (SMap ty)
rewrite (SMap smapF body collectF) = do
  body' <- transformExprM rewriteBody body
  return $ SMap smapF body' collectF
  where
    rewriteBody :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteBody (Let fun@(PureDFFun _ bnd _) cont)
      | not (isIgnorable bnd) = liftFunction (findSourceAndApply body) fun cont
    rewriteBody e = pure e

appendExpr :: NormalizedDFExpr ty
           -> NormalizedDFExpr ty
           -> NormalizedDFExpr ty
appendExpr (Let app cont) rest = Let app $ appendExpr cont rest
appendExpr Var{} rest = rest

isIgnorable :: QualifiedBinding -> Bool
isIgnorable (QualifiedBinding ["ohua", "lang"] _) = True
isIgnorable _ = False

liftFunction :: forall ty c.
                (forall a.Binding -> (forall c.DFApp c ty -> OhuaM a) -> OhuaM a)
             -> DFApp 'Fun ty
             -> NormalizedDFExpr ty
             -> OhuaM (NormalizedDFExpr ty)
liftFunction search (PureDFFun out fun inp) cont = do
  futuresBnd <- DataBinding <$> generateBindingWith "futures"
  outBound <- handleOutputSide futuresBnd
  (inBoundBnds, inBound) <- handleInbounds
  let spawned = handleFun inBoundBnds futuresBnd
  return $ inBound $ Let spawned outBound
  where
    handleOutputSide :: ABinding 'Data -> OhuaM (NormalizedDFExpr ty)
    handleOutputSide futuresBnd = do
      resultsBnd <- DataBinding <$> generateBindingWith "results"
      sizeBnd    <- DataBinding <$> generateBindingWith "size_<unused>"
      ctrlBnd    <- DataBinding <$> generateBindingWith "ctrl_<unused>"
      smapOut    <- case out of
                      Direct abnd -> pure $ DataBinding $ unwrapABnd abnd
                      _ -> unsupported "destructuring and dispatch on lifted function"
      return $
        Let
        (PureDFFun
          (Direct resultsBnd)
          "ohua.lang.data.par/join_futures"
          (DFVar TypeVar futuresBnd  :|[])
          ) $
        Let
        (PureDFFun
          (Destruct $ Direct smapOut :| [Direct ctrlBnd, Direct sizeBnd] )
          DFRef.smapFun
          (DFVar TypeVar resultsBnd  :|[])
          )
        cont

    handleFun :: NonEmpty (DFVar 'Data ty) -> ABinding 'Data -> DFApp 'Fun ty
    handleFun inBounds futuresBnd =
      PureDFFun
      (Direct futuresBnd)
      "ohua.lang.data.par/spawn_futures"
      (DFEnvVar TypeVar (FunRefLit $ FunRef fun Nothing Untyped) NE.<| inBounds)

    handleInbounds :: OhuaM ( NonEmpty (DFVar 'Data ty)
                            , NormalizedDFExpr ty -> NormalizedDFExpr ty)
    handleInbounds = do
      (inBoundBnds, inBounds) <- NE.unzip <$> mapM handleInbound inp
      return ( inBoundBnds
             , \cont -> foldr Let cont $ catMaybes $ NE.toList inBounds
             )

    handleInbound :: DFVar 'Data ty
                  -> OhuaM (DFVar 'Data ty, Maybe (DFApp 'Fun ty))
    handleInbound v@(DFVar _ty bnd) = do
      search (unwrapABnd bnd) $ \src ->
        case src of
          -- skip the SMap. (garbage collected by dead code elimination)
          (PureDFFun _ source input) | source == DFRef.smapFun -> pure (head input, Nothing)
          -- skip the Control. (garbage collected by dead code elimination)
          (PureDFFun _ source input) | source == DFRef.ctrl -> pure (last input, Nothing)
          -- collect.
          fun -> do
            collectBnd <- DataBinding <$> generateBindingWith "data_par_collect"
            return ( DFVar TypeVar collectBnd
                   , Just $
                     PureDFFun
                     (Direct collectBnd)
                     DFRef.collect
                     $ v:|[]
                   )
    handleInbound v = pure (v, Nothing)

-- assumes SSA
findSourceAndApply ::NormalizedDFExpr ty
                   -> Binding
                   -> (forall c.DFApp c ty -> OhuaM a)
                   -> OhuaM a
findSourceAndApply e bnd f = go e
  where
    go (Let app _)
      | HS.member bnd (HS.fromList $ outBindings app) = f app
    go (Let _ cont) = go cont
    go _ =
      invariantBroken $
      "Expression is not well-scoped! Binding `" <> show bnd <> "` not found."

