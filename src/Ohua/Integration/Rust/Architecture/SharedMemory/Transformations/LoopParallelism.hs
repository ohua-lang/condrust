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
        collectSmapBody (Var _) = undefined -- TODO:  throwError $ "Invariant broken: Found a smap expression not delimited by a collect"
        collectSmapBody (Let app cont) = case app of
            -- loop body has ended
            (PureDFFun _ collectFun _) -> ((Let app $ Var "__end"), cont)
            (PureDFFun _ smapFun _) -> undefined -- TODO: throwError $ "Nested smap expressions are not supported yet"
            otherwise -> let (next, cont') = collectSmapBody cont in
                             ((Let app next), cont')

        rewriteBody :: NormalizedDFExpr ty -> NormalizedDFExpr ty
        rewriteBody b = do
            let liftableFunctions = findLiftable b
            -- TODO: Actually lift the functions
            b
        
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
