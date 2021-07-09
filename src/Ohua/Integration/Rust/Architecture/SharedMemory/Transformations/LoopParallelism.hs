{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transformations.LoopParallelism where

import Ohua.Core.Prelude
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs

import Ohua.Types.Reference

liftPureFunctions :: forall ty. NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
liftPureFunctions expr = return $ locateSmap expr
    where
        locateSmap :: NormalizedDFExpr ty -> NormalizedDFExpr ty
        locateSmap e@(Let app cont) = case app of
            -- catch smapFun
            (PureDFFun _ smapFun _) -> let (smap, cont') = collectSmapBody e in
                                            combine (rewriteBody smap) (locateSmap cont')
            otherwise -> Let app (locateSmap cont)
        locateSmap v@(Var _) = v

        -- collects the body of a smap for processing
        collectSmapBody :: NormalizedDFExpr ty -> (NormalizedDFExpr ty, NormalizedDFExpr ty)
        collectSmapBody (Var _) = error $ "Invariant broken: Found a smap expression not delimited by a collect"
        collectSmapBody (Let app cont) = case app of
            -- loop body has ended
            (PureDFFun _ collectFun _) -> ((Let app $ Var "__end"), cont)
            (PureDFFun _ smapFun _) -> error $ "Nested smap expressions are not supported yet"
            otherwise -> let (next, cont') = collectSmapBody cont in
                             ((Let app next), cont')

        rewriteBody :: NormalizedDFExpr ty -> NormalizedDFExpr ty
        rewriteBody b = do
            let liftableFunctions = findLiftable b
            foldl liftFunction b liftableFunctions
            -- TODO: Optimize away now-obsolete smap functions
        
        combine :: NormalizedDFExpr ty -> NormalizedDFExpr ty -> NormalizedDFExpr ty
        combine (Let app cont) rest = Let app $ combine cont rest
        combine (Var "__end") rest = rest

        findLiftable :: NormalizedDFExpr ty -> [DFApp 'Fun ty]
        findLiftable (Var _) = []
        findLiftable (Let app cont) = case app of
            (PureDFFun _ binding _) -> if isIgnorable binding
                                         then findLiftable cont
                                         else (app : findLiftable cont)
            otherwise -> findLiftable cont

        isIgnorable :: QualifiedBinding -> Bool
        isIgnorable (QualifiedBinding (NSRef [(Binding "ohua"), (Binding "lang")]) _) = True
        isIgnorable _ = False

        liftFunction :: DFApp 'Fun ty -> NormalizedDFExpr ty -> NormalizedDFExpr ty
        liftFunction fun e@(Let smap cont) = do
            let matchingArgs = findMatchingArgs fun smap
            -- prepend Pool creation
            let expr = Let (createPool "pool") e
            insertFunctions expr fun matchingArgs "pool"

        -- Finds all bindings that are outputs from smap and inputs to the pure function -> these will be collected
        findMatchingArgs :: DFApp 'Fun ty -> DFApp 'Fun ty -> [ABinding 'Data]
        findMatchingArgs (PureDFFun _ _ inp) (PureDFFun outp _ _) = do
            let inpBindings = mapMaybe (\case
                                            (DFVar _ bnd) -> Just bnd
                                            otherwise -> Nothing
                                       ) inp
            let outpBindings = collectOutBindings outp
            intersect inpBindings outpBindings
                where
                    collectOutBindings :: OutData 'Data -> [ABinding 'Data]
                    collectOutBindings (Direct b) = [b]
                    collectOutBindings (Destruct lst) = map (\(acc, item) -> acc ++ collectOutBindings item) [] lst
                    collectOutBindings (Dispatch lst) = map (\(acc, item) -> acc ++ collectOutBindings item) [] lst

        createPool :: Binding -> DFApp 'Fun ty
        createPool name = PureDFFun (Direct $ DataBinding name) (QualifiedBinding ["Pool"] "new") ((DFEnvVar TypeVar UnitLit) :| [])

        insertFunctions :: NormalizedDFExpr ty -> DFApp 'Fun ty -> [ABinding 'Data] -> Binding -> NormalizedDFExpr ty
        insertFunctions (Let app cont) fun binds poolName
            | app == fun = do
                -- insert collect, split, pool spawn & pool collect as well as a new smap (use the result binding from the current function asbinding for that
                --let collectedBindings = undefined -- TODO: generate bindings based on `binds` for the output of 'collect'
                --let collectCall = buildCollects collectedBindings binds
                undefined
            | otherwise = Let app $ insertFunctions cont fun binds poolName
