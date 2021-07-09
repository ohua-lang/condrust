{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transformations.LoopParallelism where

import Ohua.Core.Prelude
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs

import Ohua.Types.Reference
import Ohua.Types.Literal

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

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
        collectSmapBody (Var _) = error "Invariant broken: Found a smap expression not delimited by a collect"
        collectSmapBody (Let app cont) = case app of
            -- loop body has ended
            (PureDFFun _ collectFun _) -> ((Let app $ Var "__end"), cont)
            (PureDFFun _ smapFun _) -> error "Nested smap expressions are not supported yet"
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

        liftFunction :: DFApp 'Fun ty
                     -> (DFApp 'Fun ty, NormalizedDFExpr ty)
                     -> NormalizedDFExpr ty
        liftFunction fun (smap,cont) =
          let matchingArgs = findMatchingArgs fun smap
              -- prepend Pool creation
              expr = Let (createPool "pool") $ Let smap cont
              sizeChan =
                case smap of
                  (PureDFFun (Destruct out) smapFun _) ->
                    case last out of
                      (Direct (DataBinding db)) -> db
                      otherwise -> error "Incorrect smap function return type"
                  otherwise -> error "Internal error: Expected smap function"
          in insertFunctions expr fun matchingArgs sizeChan "pool"

        -- Finds all bindings that are outputs from smap and inputs to the pure function -> these will be collected
        findMatchingArgs :: DFApp 'Fun ty -> DFApp 'Fun ty -> [DFVar 'Data ty]
        findMatchingArgs (PureDFFun _ _ inp) (PureDFFun outp _ _) =
            let inpBindings = mapMaybe (\case
                                            d@DFVar{} -> Just d
                                            otherwise -> Nothing
                                       ) $ NE.toList inp
                outpBindings = findInput outp-- collectOutBindings outp
            in L.intersect inpBindings outpBindings
              where
                findInput :: OutData b -> [DFVar 'Data ty]
                findInput = undefined
                -- maybe this function is a bit stupid given the structure of smap, but oh well
                collectOutBindings :: OutData b -> [ABinding b]
                collectOutBindings (Direct b) = [b]
                collectOutBindings (Destruct lst) = map (\(acc, item) -> acc ++ collectOutBindings item) [] lst
                collectOutBindings (Dispatch lst) = map (\(acc, item) -> acc ++ collectOutBindings item) [] lst

        createPool :: Binding -> DFApp 'Fun ty
        createPool name = PureDFFun (Direct $ DataBinding name) (QualifiedBinding ["pool"] "new") ((DFEnvVar TypeVar UnitLit) :| [])

        insertFunctions :: NormalizedDFExpr ty
                        -> DFApp 'Fun ty
                        -> [ABinding 'Data]
                        -> Binding
                        -> Binding
                        -> NormalizedDFExpr ty
        insertFunctions (Let app@(PureDFFun fnOut fnName fnIn) cont) fun binds sizeChan poolName
            | app == fun =
                -- insert collect, split, pool spawn & pool collect as well as a new smap
                -- (use the result binding from the current function as binding for that)
                let collectedBindings = buildBindings binds "_0" -- generate bindings based on `binds` for the output of 'collect'
                    collectCalls = buildCollects collectedBindings binds sizeChan
                    splitDataNames = buildBindings binds "_split"
                    splits = buildSplits splitDataNames collectedBindings
                    alteredPoolName = poolName ++ "_0"
                -- TODO: add FunRefLit (?) at top of the list!
                    argList = map (replaceArgument $ zip binds splits) fnIn
                    spawn = PureDFFun (Direct $ DataBinding alteredPoolName) (QualifiedBinding ["pool"] "spawn") argList
                -- TODO: Ensure argument list is length 1!
                    alteredPoolName2 = poolName ++ "_1"
                    join = PureDFFun (Direct $ DataBinding alteredPoolName2) (QualifiedBinding ["pool"] "join") ((DFVar TypeVar  (DataBinding alteredPoolName)) :| [])
                    newSmapCall = undefined
                in foldr (\(app,cnt) -> Let app cnt) cont (collectCalls ++ splits ++ spawn ++ join ++ newSmapCall)
            | otherwise = Let app $ insertFunctions cont fun binds poolName

        buildCollects :: [Binding] -> [DFVar 'Data ty] -> DFVar 'Data ty -> [DFApp 'Fun ty]
        buildCollects (out:outs) (inp:inps) sizeChan =
          (PureDFFun (Direct $ DataBinding out) collect $ inp :| [(DFVar (Type ty) (DataBinding sizeChan))]) : buildCollects outs inps sizeChan
        buildCollects [] [] _ = []
        buildCollects _ _ _ = error "Internal error: Got incomplete list of bindings for pure function lifting"

        buildSplits :: [Binding] -> [Binding] -> [DFApp 'Fun ty]
        buildSplits (out:outs) (inp:inps) = (PureDFFun (Direct $ DataBinding out) (QualifiedBinding [] "split") $ ((DFVar (Type ty) (DataBinding inp)) :| [(DFEnvVar (Type ty) (Lit 4))])) : buildSplits outs inps
        buildSplits [] [] _ = []
        buildSplits _ _ _ = error "Internal error: Got incomplete list of bindings for pure function lifting"

        buildBindings :: [ABinding a] -> String -> [Binding]
        buildBindings ((DataBinding (Binding name)):xs) suffix = (Binding $ toText $ (toString name) ++ suffix) : buildBindings xs suffix
        buildBindings ((StateBinding _):_) _ = error "Reached state binding in data binding list"
        buildBindings [] _ = []

        replaceArgument :: [(ABinding a, Binding)] -> DFVar a ty -> DFVar a ty
        replaceArgument ((old,new):rest) arg@(DFVar tp bnd)
          | bnd == old = DFVar tp $ DataBinding new
          | otherwise = replaceArgument rest arg
        replaceArgument [] arg = arg
