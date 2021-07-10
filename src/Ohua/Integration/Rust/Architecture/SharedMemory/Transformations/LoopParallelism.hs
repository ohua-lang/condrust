{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transformations.LoopParallelism where

import Ohua.Core.Prelude
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs

import Ohua.Types.Reference

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE


dataPar :: CustomPasses
dataPar = CustomPasses pure pure liftPureFunctions

invariantBroken :: Text -> OhuaM a
invariantBroken s = throwError $ "Invariant broken: " <> s

unsupported :: Text -> OhuaM a
unsupported s = throwError $ "Not supported: " <> s


liftPureFunctions :: forall ty.
                     NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
liftPureFunctions = locateSmap
  where
    locateSmap :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    locateSmap e@(Let app cont) =
      case app of
        -- catch smapFun
        (PureDFFun _ fn _)
          | fn == smapFun -> do
              (smap, cont') <- collectSmapBody e
              smap'         <- rewriteBody smap
              cont''        <- locateSmap cont'
              combine smap' cont''
        _ -> Let app <$> locateSmap cont
    locateSmap v = pure v

    -- collects the body of a smap for processing
    collectSmapBody :: NormalizedDFExpr ty
                    -> OhuaM (NormalizedDFExpr ty, NormalizedDFExpr ty)
    collectSmapBody (Var _) =
      invariantBroken
      "Found a smap expression not delimited by a collect"
    collectSmapBody (Let app cont) =
      case app of
        -- loop body has ended
        (PureDFFun _ fn _)
          | fn == collect -> pure (Let app $ Var "__end", cont)
          | fn == smapFun ->
              unsupported "Nested smap expressions"
        _ -> do (next, cont') <- collectSmapBody cont
                return (Let app next, cont')

    rewriteBody :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
    rewriteBody (Let smap@PureDFFun{} b) =
      let liftableFunctions = findLiftable b
       in do
        (_, rewritten) <-
            foldM liftFunction (smap,b) liftableFunctions
        pure $ Let smap rewritten
    rewriteBody _ = invariantBroken "smapFun not found"
      -- TODO: Optimize away now-obsolete smap functions

    combine :: NormalizedDFExpr ty
            -> NormalizedDFExpr ty
            -> OhuaM (NormalizedDFExpr ty)
    combine (Let app cont) rest = Let app <$> combine cont rest
    combine (Var "__end") rest = pure rest
    combine _ _ =
      invariantBroken "Expression ends with unexpected var."

    findLiftable :: NormalizedDFExpr ty -> [DFApp 'Fun ty]
    findLiftable (Var _) = []
    findLiftable (Let app cont) =
      case app of
        (PureDFFun _ bnd _) ->
          if isIgnorable bnd
          then findLiftable cont
          else app : findLiftable cont
        _ -> findLiftable cont

    isIgnorable :: QualifiedBinding -> Bool
    isIgnorable (QualifiedBinding (NSRef [(Binding "ohua"), (Binding "lang")]) _) = True
    isIgnorable _ = False

    liftFunction :: (DFApp 'Fun ty, NormalizedDFExpr ty)
                 -> DFApp 'Fun ty
                 -> OhuaM (DFApp 'Fun ty, NormalizedDFExpr ty)
    liftFunction (smap@(PureDFFun outSMap _ _), cont) fun =
      let matchingArgs = findMatchingArgs fun smap
          -- prepend Pool creation
          poolExpr = Let (createPool "pool") cont
          sizeChan =
            DFVar TypeVar $ DataBinding $ last $ outBnds outSMap
      in do
        fs <- insertFunctions poolExpr fun matchingArgs sizeChan "pool"
        return (smap, fs)

        -- Finds all bindings that are outputs from smap and inputs to the pure function -> these will be collected
    findMatchingArgs :: DFApp 'Fun ty
                     -> DFApp 'Fun ty
                     -> [DFVar 'Data ty]
    findMatchingArgs (PureDFFun _ _ inp) (PureDFFun outp _ _) =
      let inpBindings = mapMaybe (\case
                                     d@DFVar{} -> Just d
                                     _ -> Nothing
                                 ) $ NE.toList inp
          outpBindings = findInput outp
      in L.intersectBy
         (\(DFVar _ inp') (DFVar _ out) -> inp' == out)
         inpBindings
         outpBindings
      where
        findInput :: OutData b -> [DFVar 'Data ty]
        findInput (Direct b@(DataBinding _)) = [DFVar TypeVar b]
        findInput (Direct _) = []
        findInput (Destruct lst) =
          foldl (\acc item -> acc ++ findInput item) [] lst
        findInput (Dispatch lst) =
          mapMaybe (\case
                       b@DataBinding{} -> Just $ DFVar TypeVar b
                       _ -> Nothing
                   ) $ NE.toList lst

    createPool :: Binding -> DFApp 'Fun ty
    createPool bnd =
      PureDFFun
      (Direct $ DataBinding bnd)
      (QualifiedBinding ["pool"] "new")
      ((DFEnvVar TypeVar UnitLit) :| [])

    insertFunctions :: NormalizedDFExpr ty
                    -> DFApp 'Fun ty
                    -> [DFVar 'Data ty]
                    -> DFVar 'Data ty
                    -> String
                    -> OhuaM (NormalizedDFExpr ty)
    insertFunctions (Let app@(PureDFFun fnOut fnName fnIn) cont) fun binds sizeChan poolName
      | fnDFApp app == fnDFApp fun =
        -- insert collect, split, pool spawn & pool collect as well as a new smap
        -- (use the result binding from the current function as binding for that)
          let collectedBindings = buildBindings binds "_0" -- generate bindings based on `binds` for the output of 'collect'
              splitDataNames = buildBindings binds "_split"
              alteredPoolName = Binding $ toText $ poolName ++ "_0"
                -- TODO: add FunRefLit (?) at top of the list!
              argList =
                map (replaceArgument $ zip binds splitDataNames) fnIn
              spawn =
                PureDFFun
                (Direct $ DataBinding alteredPoolName)
                (QualifiedBinding ["pool"] "spawn")
                argList
                -- TODO: Ensure argument list is length 1!
              alteredPoolName2 = Binding $ toText $ poolName ++ "_1"
              join =
                PureDFFun
                (Direct $ DataBinding alteredPoolName2)
                (QualifiedBinding ["pool"] "join")
                ((DFVar TypeVar $ DataBinding alteredPoolName) :| [])
                -- TODO: Actually create the smap call
              newSmapCall = undefined
          in do
            cc <- buildCollects
                  (extractBndsFromInputs collectedBindings)
                  binds
                  sizeChan
            sps <- buildSplits
                   (extractBndsFromInputs splitDataNames)
                   collectedBindings
            pure $ foldr
                   (\app cnt -> Let app cnt)
                   cont
                   (cc ++
                    sps ++
                    [spawn] ++
                    [join] ++
                    [newSmapCall])
      | otherwise =
        Let app <$> insertFunctions cont fun binds sizeChan poolName

    buildCollects :: [Binding]
                  -> [DFVar 'Data ty]
                  -> DFVar 'Data ty
                  -> OhuaM [DFApp 'Fun ty]
    buildCollects (out:outs) (inp:inps) sizeChan =
      ((PureDFFun
        (Direct $ DataBinding out)
        collect
        $ inp :| [sizeChan]) :)
      <$> buildCollects outs inps sizeChan
    buildCollects [] [] _ = pure []
    buildCollects _ _ _ =
      invariantBroken "Got incomplete list of bindings for pure function lifting"

    buildSplits :: [Binding] -> [DFVar 'Data ty]
                -> OhuaM [DFApp 'Fun ty]
    buildSplits (out:outs) (inp:inps) =
      ((PureDFFun
        (Direct $ DataBinding out)
        (QualifiedBinding [] "split")
        $ inp :| [(DFEnvVar TypeVar (NumericLit 4))]) :)
      <$> buildSplits outs inps
    buildSplits [] [] = pure []
    buildSplits _ _ =
      invariantBroken "Got incomplete list of bindings for pure function lifting"

    buildBindings :: [DFVar 'Data ty] -> String -> [DFVar 'Data ty]
    buildBindings (DFVar t (DataBinding (Binding name)):xs) suffix =
      (DFVar
       t
       (DataBinding (Binding $ toText $ (toString name) ++ suffix)))
      : buildBindings xs suffix
    buildBindings [] _ = []

    replaceArgument :: [(DFVar 'Data ty, DFVar 'Data ty)]
                    -> DFVar 'Data ty -> DFVar 'Data ty
    replaceArgument ((old,new):rest) arg
      | arg == old = new
      | otherwise = replaceArgument rest arg
    replaceArgument [] arg = arg
