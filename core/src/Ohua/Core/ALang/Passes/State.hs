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

module Ohua.Core.ALang.Passes.State where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Util
import Ohua.Core.ALang.Passes.SSA
import qualified Ohua.Core.ALang.Refs as Refs

import qualified Data.HashSet as HS

import Control.Lens.Combinators (over)
import Control.Lens.Plated (plate)

-- TODO recursion support is still pending here!

preControlPasses :: MonadOhua m => Expression -> m Expression
preControlPasses = addCtxtExit >=> performSSA

postControlPasses :: Expression -> Expression
postControlPasses = splitCtrls . transformCtxtExits

runSTCLangSMapFun :: Expression
runSTCLangSMapFun = Lit $ FunRefLit $ FunRef Refs.runSTCLangSMap Nothing

runSTCLangIfFun :: Expression
runSTCLangIfFun = Lit $ FunRefLit $ FunRef Refs.runSTCLangIf Nothing

-- invariant: this type of node has at least one var as input (the computation result)
ctxtExit :: QualifiedBinding
ctxtExit = "ohua.lang/ctxtExit"

ctxtExitFunRef :: Expression
ctxtExitFunRef = Lit $ FunRefLit $ FunRef ctxtExit Nothing

addCtxtExit :: MonadGenBnd m => Expression -> m Expression
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
                                Let missingState (PureFunction Refs.id Nothing `Apply` Var missingState) 
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
    
        f (Let v (fun@(PureFunction op _) `Apply` expr `Apply` ds) cont)
            | op == Refs.smap =
                let states = collectStates expr
                in do
                    expr' <- mkST (map Var states) expr
                    ctxtOut <- generateBindingWith "ctxt_out"
                    return $
                        Let ctxtOut (fun `Apply` expr' `Apply` ds) $
                        mkDestructured (v:states) ctxtOut
                        cont
        f expr = return expr

-- Assumptions: This function is applied to a normalized and SSAed expression.
--              This transformation executes after the control rewrites.
transformCtxtExits :: Expression -> Expression
transformCtxtExits = evictOrphanedDestructured . f
    where
        f :: Expression -> Expression
        f (Let v e@(PureFunction op _ `Apply` _) cont)
            | op == ctxtExit =
                let (_, compOut:stateArgs) = fromApplyToList e
                in descend (g v compOut stateArgs) cont
        f e = descend f e

        g :: Binding -> Expression -> [Expression] -> Expression -> Expression
        g compound compOut stateArgs (Let v e@(PureFunction op _ `Apply` _) cont)
            | op == ctxtExit = 
                -- Must be a conditional
                let (_, compOut':stateArgs') = fromApplyToList e
                in descend (h compound compOut stateArgs v compOut' stateArgs') cont

        g _ compOut stateOuts (Let v (fun@(PureFunction op _) `Apply` size `Apply` _) cont)
            | op == Refs.collect = 
                let (compOut':stateOuts') = findDestructured cont v
                    stateExits ct = 
                        foldr
                            (\(s',s) c -> 
                                Let s' (runSTCLangSMapFun `Apply` size `Apply` s) c)
                            ct
                            $ zip stateOuts' stateOuts 

                in Let compOut' (fun `Apply` size `Apply` compOut) $
                    stateExits $
                    descend f cont
        g co c s e = descend (g co c s) e

        h :: Binding -> Expression -> [Expression]
          -> Binding -> Expression -> [Expression]
          -> Expression
          -> Expression
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

splitCtrls :: Expression -> Expression
splitCtrls = go
    where
        go :: Expression -> Expression
        go (Let v e@(f@(PureFunction op _) `Apply` ctrlSig `Apply` _) cont) | op == Refs.ctrl = 
            let (_, _ctrlIn:vars) = fromApplyToList e
                outs = findDestructured cont v
            in foldr
                (\(varOut,varIn) c -> 
                    Let varOut (f `Apply` ctrlSig `Apply` varIn) c)
                cont $
                zip outs vars
        go e = e

pattern NthFunction :: Binding -> Expression
pattern NthFunction bnd <- PureFunction "ohua.lang/nth" _ `Apply` _ `Apply` _ `Apply` Var bnd

evictOrphanedDestructured :: Expression -> Expression
evictOrphanedDestructured e = 
    let allBnds = HS.fromList [v | Let v _ _ <- universe e]
    in transform (f allBnds) e
    where 
        f :: HS.HashSet Binding -> Expression -> Expression
        f bnds (Let _v (NthFunction bnd) cont) | not $ HS.member bnd bnds = cont
        f _ expr = expr

findDestructured :: Expression -> Binding -> [Binding]
findDestructured e bnd = 
    map snd $
    sort 
        [ (i,v) 
        | Let v 
            (PureFunction "ohua.lang/nth" _ `Apply` 
                Lit (NumericLit i) `Apply` 
                _ `Apply` 
                Var bnd')
            _  
            <- universe e
        , bnd == bnd']

applyToBody :: (Expression -> Expression) -> Expression -> Expression
applyToBody f (Lambda _ body) = applyToBody f body
applyToBody f e = f e

collectStates :: Expression -> [Binding]
collectStates e = [ s | (BindState (Var s) _) <- universe e ] 

mkST :: (MonadGenBnd m) => [Expression] -> Expression -> m Expression
mkST states = \case
    Let v e res -> Let v e <$> mkST states res
    e -> do
        allOut <- generateBindingWith "all_out" 
        return $
            Let allOut
                (mkApply ctxtExitFunRef $ e : states)
                (Var allOut)
