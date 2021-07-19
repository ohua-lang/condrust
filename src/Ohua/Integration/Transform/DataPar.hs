{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Transform.DataPar where

import Ohua.Core.Prelude hiding (length, rewrite)
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.PPrint (prettyExprM)
import qualified Ohua.Core.DFLang.Refs as DFRef

import qualified Ohua.Backend.Lang as B

import Ohua.Types.Reference

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS
import Data.Functor.Foldable (cata, embed)


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

spawnFutures :: QualifiedBinding
spawnFutures = "ohua.lang/spawn_futures"

joinFutures :: QualifiedBinding
joinFutures = "ohua.lang/collectFuture"

liftPureFunctions :: forall ty.
                     NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
liftPureFunctions = rewriteSMap
  where
    rewriteSMap :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteSMap (Let app cont) =
      case app of
        (PureDFFun _ fn _)
          | fn == DFRef.smapFun ->
              let rewriteIt smap@(SMap _ smapBody _) = do
                    smap'@(SMap app' smapBody' coll') <- rewrite smap
                    case length smapBody of
                      -- TODO a stronger termination metric would be better
                      l | not (l == length smapBody') -> rewriteIt smap'
                      _ -> pure $ \c -> Let app' $ appendExpr smapBody' $ Let coll' c
              in do
                (smapBody, coll, cont') <- collectSMap cont
                rewritten <- rewriteIt $ SMap app smapBody coll
                rewritten <$> rewriteSMap cont'
        _ -> Let app <$> rewriteSMap cont
    rewriteSMap v = pure v

    -- collects the body of an smap for processing
    collectSMap :: NormalizedDFExpr ty
                -> OhuaM (NormalizedDFExpr ty, DFApp 'Fun ty, NormalizedDFExpr ty)
    collectSMap Var{} =
      invariantBroken
      "Found an smap expression not delimited by a collect"
    collectSMap (Let app cont) =
      case app of
        -- loop body has ended
        (PureDFFun _ fn (_ :|[DFVar _ result]))
          | fn == DFRef.collect ->
            pure (Var $ unwrapABnd result, app, cont)
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
      | not (isIgnorable bnd) =
          liftFunction fun cont
    rewriteBody e = pure e

appendExpr :: NormalizedDFExpr ty
           -> NormalizedDFExpr ty
           -> NormalizedDFExpr ty
appendExpr (Let app cont) rest = Let app $ appendExpr cont rest
appendExpr Var{} rest = rest

isIgnorable :: QualifiedBinding -> Bool
isIgnorable (QualifiedBinding ["ohua","lang"] _) = True
isIgnorable _ = False

liftFunction :: forall ty.
                DFApp 'Fun ty
             -> NormalizedDFExpr ty
             -> OhuaM (NormalizedDFExpr ty)
liftFunction (PureDFFun out fun inp) cont = do
  futuresBnd <- DataBinding <$> generateBindingWith "futures"
  outBound <- handleOutputSide futuresBnd
  let spawned = handleFun futuresBnd
  return $ Let spawned outBound
  where
    handleOutputSide :: ABinding 'Data -> OhuaM (NormalizedDFExpr ty)
    handleOutputSide futuresBnd = do
      return $
        Let
        (PureDFFun
          out
          joinFutures
          (DFVar TypeVar futuresBnd  :|[])
          )
        cont

    handleFun :: ABinding 'Data -> DFApp 'Fun ty
    handleFun futuresBnd =
      PureDFFun
      (Direct futuresBnd)
      spawnFutures
      (DFEnvVar TypeVar (FunRefLit $ FunRef fun Nothing Untyped) NE.<| inp)


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
      | qb == spawnFutures -> embed e
      -- there is nothing to be done here.
      -- because we can implement this function easily in Rust.
    e@(B.ApplyF (B.Stateless qb _args))
      | qb == joinFutures -> embed e
    e -> embed e


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
--  let (inputs',rest) = takeN inputs in
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
--             let (_,[state',someState']) = smap \input ->
--                                       let x = t_before input in
--                                       let (y,state') = state.g x in
--                                       let z = t_after y in
--                                       (z,state',someState')
--                                     inputs in
--             let a = t_inbetween someState' in
--             let b = state'.check a in
--             if b
--             then f state' a
--             else state'
-- @
--
amorphous :: NormalizedDFExpr ty -> NormalizedDFExpr ty
amorphous = undefined
