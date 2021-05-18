{-|
Module      : $Header$
Description : Implementation of state transformations.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : EPL-1.0
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Core.ALang.Passes.State where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Util
import Ohua.Core.ALang.Passes.SSA
import qualified Ohua.Core.ALang.Refs as Refs

import Ohua.Core.ALang.PPrint ()

import qualified Data.HashSet as HS

import Control.Lens.Combinators (over)
import Control.Lens.Plated (plate)


-- TODO recursion support is still pending here!

preControlPasses :: MonadOhua m => Expr ty -> m (Expr ty)
preControlPasses =
  transformIntoStateThreads >=>
  addCtxtExit >=>
  performSSA

-- FIXME there is code missing that turns the basic state threads into real state threads! this is the very first transformation that needs to run.
-- the transformation is: make state threads and then rebind the state -> alpha renaming on the rest of the term.

postControlPasses :: Expr ty -> Expr ty
postControlPasses = transformCtxtExits

runSTCLangSMapFun :: Expr ty
runSTCLangSMapFun = Lit $ FunRefLit $ FunRef Refs.runSTCLangSMap Nothing Untyped

runSTCLangIfFun :: Expr ty
runSTCLangIfFun = Lit $ FunRefLit $ FunRef Refs.runSTCLangIf Nothing Untyped

-- invariant: this type of node has at least one var as input (the computation result)
ctxtExit :: QualifiedBinding
ctxtExit = "ohua.lang/ctxtExit"

ctxtExitFunRef :: Expr ty
ctxtExitFunRef = Lit $ FunRefLit $ FunRef ctxtExit Nothing Untyped

-- | Transforms every stateful function into a fundamental state thread.
--   Corrects the reference to the state for the rest of the computation.
--   Assumption: expression is in ANF form (TODO enforce via the type system)
transformIntoStateThreads :: (MonadOhua m) => Expr ty -> m (Expr ty)
transformIntoStateThreads = transformM f
  where
    f e@(Let v app cont) = do
      s <- stBndForStatefulFun app
      let s' = used cont =<< s
      maybe (return e) (g v app cont) s'
    f expr = return expr

    used cont b@(bnd, _) =
      case [b | Var b <- universe cont, b == bnd] of
        [] -> Nothing
        _ -> Just b

    stBndForStatefulFun (StatefulFunction _ _ (Var bnd)) = Just . (bnd,) <$> generateBindingWith bnd
    -- FIXME Once again, this stupid over-generalization of the language is a pain!
    stBndForStatefulFun e@(StatefulFunction _ _ _) = error $ "state should have been var: " <> show e
    stBndForStatefulFun (e1 `Apply` _)            = stBndForStatefulFun e1
    stBndForStatefulFun _                         = return Nothing

    g r app cont (stateIn,stateOut) = do
      x <- generateBinding
      return $
        Let x app $
        destructure (Var x) [stateOut,r] $
        substitute stateIn (Var stateOut) cont

-- TODO do not introduce orphaned state, i.e., state that is never used again.
--      check before that which of the state is actually used after the control context it is used in.
addCtxtExit :: MonadGenBnd m => Expr ty -> m (Expr ty)
addCtxtExit = transformM f
    where
        f (Let v (fun@(PureFunction op _) `Apply` trueBranch `Apply` falseBranch) cont)
            | op == Refs.ifThenElse =
                let trueBranchStates = HS.fromList $ collectStates trueBranch
                    falseBranchStates = HS.fromList $ collectStates falseBranch

                    trueBranchStatesMissing = HS.difference falseBranchStates trueBranchStates
                    falseBranchStatesMissing = HS.difference trueBranchStates falseBranchStates
                    addMissing =
                        HS.foldr
                            (\missingState c ->
                                Let missingState (pureFunction Refs.id Nothing (FunType [TypeVar] )`Apply` Var missingState)
                                    c)
                    trueBranch' = applyToBody (`addMissing` trueBranchStatesMissing) trueBranch
                    falseBranch' = applyToBody (`addMissing` falseBranchStatesMissing) falseBranch

                    allStates = HS.toList $ HS.union trueBranchStates falseBranchStates
                in do
                    trueBranch'' <- mkST (map Var allStates) trueBranch'
                    falseBranch'' <- mkST (map Var allStates) falseBranch'
                    ctxtOut <- generateBindingWith "ctxt_out"
                    return $
                        Let ctxtOut (fun `Apply` trueBranch'' `Apply` falseBranch'') $
                        mkDestructured (v:allStates) ctxtOut
                        cont

        f (Let v (fun@(PureFunction op _) `Apply` lam `Apply` ds) cont)
            | op == Refs.smap =
                let (args, expr) = lambdaArgsAndBody lam
                    states = collectStates expr
                in do
                    expr' <- mkST (map Var states) expr
                    ctxtOut <- generateBindingWith "ctxt_out"
                    let lam' = mkLambda args expr'
                    return $
                        Let ctxtOut (fun `Apply` lam' `Apply` ds) $
                        mkDestructured (v:states) ctxtOut
                        cont
        f expr = return expr

-- Assumptions: This function is applied to a normalized and SSAed expression.
--              This transformation executes after the control rewrites.
-- Note: The use of descend is entirely not necessary anymore once the "applicative normal form"
--       is properly defined as a type.
transformCtxtExits :: forall ty. Expr ty -> Expr ty
transformCtxtExits = evictOrphanedDestructured . f
    where
        f :: Expr ty -> Expr ty
        f (Let v e@(PureFunction op _ `Apply` _) cont)
            | op == ctxtExit =
                let (_, compOut:stateArgs) = fromApplyToList e
                    g' = g v compOut stateArgs
                    cont' = g' cont
                in descend g' cont'
        f e = descend f e

        g :: Binding -> Expr ty -> [Expr ty] -> Expr ty -> Expr ty
        g compound compOut stateArgs (Let v e@(PureFunction op _ `Apply` _) cont)
            | op == ctxtExit = 
                -- Must be a conditional
                let (_, compOut':stateArgs') = fromApplyToList e
                    h' = h compound compOut stateArgs v compOut' stateArgs'
                    cont' = h' cont
                in descend h' cont'

        g ctxtExitRes compOut stateOuts (Let v (fun@(PureFunction op _) `Apply` size `Apply` (Var exitRes)) cont)
            | op == Refs.collect && ctxtExitRes == exitRes = 
                let (compOut':stateOuts') = findDestructured cont v
                    stateExits ct = 
                        foldr
                            (\(s',s) c -> 
                                Let s' (runSTCLangSMapFun `Apply` size `Apply` s) c)
                            ct
                            $ zip stateOuts' stateOuts 
                in descend f $
                    Let compOut' (fun `Apply` size `Apply` compOut) $
                        stateExits 
                        cont
        g co c s e = descend (g co c s) e

        h :: Binding -> Expr ty -> [Expr ty]
          -> Binding -> Expr ty -> [Expr ty]
          -> Expr ty
          -> Expr ty
        h compound compOut stateOuts _compound' compOut' stateOuts' 
            (Let v (fun@(PureFunction op _) `Apply` cond `Apply` Var trueBranch `Apply` _falseBranch) cont)
            | op == Refs.select = 
                let (tbCompOut, tbStateArgs, fbCompOut, fbStateArgs) =
                        if compound == trueBranch
                        then (compOut, stateOuts, compOut', stateOuts')
                        else (compOut', stateOuts', compOut, stateOuts)
                    (compOut'':stateOuts'') = findDestructured cont v
                    stateExits ct = 
                        foldr
                            (\(s', (ts,fs)) c -> 
                                Let s' (runSTCLangIfFun `Apply` cond `Apply` ts `Apply` fs) c)
                            ct $ 
                            zip stateOuts'' $ 
                            zip tbStateArgs fbStateArgs 
                in stateExits $
                    Let compOut'' (fun `Apply` cond `Apply` tbCompOut `Apply` fbCompOut) $
                    descend f cont
        h co c s co' c' s' e = descend (h co c s co' c' s') e

        descend = over plate -- note composOp = descend = over plate -> https://www.stackage.org/haddock/lts-14.25/lens-4.17.1/Control-Lens-Plated.html#v:para (below)

applyToBody :: (Expr ty -> Expr ty) -> Expr ty -> Expr ty
applyToBody f (Lambda _ body) = applyToBody f body
applyToBody f e = f e

collectStates :: Expr ty -> [Binding]
collectStates e = [ s | (BindState (Var s) _) <- universe e ] 

mkST :: (MonadGenBnd m) => [Expr ty] -> Expr ty -> m (Expr ty)
mkST states = \case
    Let v e res -> Let v e <$> mkST states res
    e -> do
        allOut <- generateBindingWith "all_out" 
        return $
            Let allOut
                (mkApply ctxtExitFunRef $ e : states)
                (Var allOut)
