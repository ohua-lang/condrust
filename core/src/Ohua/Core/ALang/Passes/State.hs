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
import qualified Ohua.Core.ALang.Refs as Refs

import Data.List.NonEmpty as NE
import qualified Data.HashMap.Lazy as HM


runSTCLangFun :: Expression
runSTCLangFun = Lit $ FunRefLit $ FunRef "ohua.lang/runSTCLang" Nothing

-- invariant: this type of node has at least one var as input (the computation result)
ctxtExit :: QualifiedBinding
ctxtExit = "ohua.lang/ctxtExit"

ctxtExitFunRef :: Expression
ctxtExitFunRef = Lit $ FunRefLit $ FunRef ctxtExit Nothing

addCtxtExit :: MonadGenBnd m => Expression -> m Expression
addCtxtExit = transform f
    where
        f (Let v (f@(PureFunction op _) `Apply` trueBranch `Apply` falseBranch) cont)
            | op == Refs.ifThenElse = 
                let trueBranchStates = collectStates trueBranch
                    falseBranchStates = collectStates falseBranch

                    trueBranchStatesMissing = HS.difference falseBranchStates trueBranchStates
                    falseBranchStatesMissing = HS.difference trueBranchStates falseBranchStates
                    addMissing = 
                        foldr 
                            (\cont missingState -> 
                                Let missingState (PureFunction Refs.id Nothing `Apply` missingState) 
                                    cont) 
                    trueBranch' = applyToBody (`addMissing` trueBranchStatesMissing) trueBranch
                    falseBranch' = applyToBody (`addMissing` falseBranchStatesMissing) falseBranch

                    allStates = map Var $ HS.toList $ HS.union trueBranchStates falseBranchStates
                in do 
                    trueBranch'' <- mkST allStates trueBranch'
                    falseBranch'' <- mkST allStates falseBranch'
                    ctxtOut <- generateBindingWith "ctxt_out"
                    return $
                        Let ctxtOut (f `Apply` trueBranch'' `Apply` falseBranch'') $
                        mkDestructured (v:states) ctxtOut
                        cont
    
        f (Let v (f@(PureFunction op _) `Apply` expr `Apply` ds) cont)
            | op == Refs.smap =
                let states = collectStates expr
                in do
                    expr' <- mkST states expr
                    ctxtOut <- generateBindingWith "ctxt_out"
                    return $
                        Let ctxtOut (f `Apply` expr' `Apply` ds) $
                        mkDestructured (v:states) ctxtOut
                        cont
        f e = e

-- Assumptions: This function is applied to a normalized and SSAed expression.
--              This transformation executes after the control rewrites.
transformCtxtExits :: Expression -> Expression
transformCtxtExits = f
    where
        f (Let v e@(PureFunction op _ `Apply` _) cont)
            | op == ctxtExit =
                let (_, compOut:stateArgs) = fromApplyToList e
                in descend (g compOut stateArgs) cont
        f = descend f

        g compOut stateOuts (Let v e@(PureFunction op i `Apply` _) cont)
            | op == ctxtExit = 
                -- Must be a conditional
                let (_, compOut':stateArgs') = fromApplyToList e
                in descend (h compOut stateArgs compOut' stateArgs') cont

        g compOut stateOuts (Let v (f@(PureFunction op i) `Apply` size `Apply` _) cont)
            | op == Refs.collect = 
                let (compOut':stateOuts') = findDestructured cont
                    stateExits ct = 
                        foldr
                            (\((s',s), c) -> 
                                Let s' (runSTCLangFun `Apply` size `Apply` s)) c
                            ct
                            $ zip stateOuts' stateOuts 

                in Let compOut' 
                        (f `Apply` size `Apply` compOut) $
                        stateExits $
                        descend f cont
        g c s e = descend (g c s) e

        g compOut stateOuts compOut' stateOuts' (Let v (f@(PureFunction op _) `Apply` _ `Apply` _) cont)
            | op == Refs.select = undefined -- TODO


pattern NthFunction :: Binding -> Expression
pattern NthFunction bnd = PureFunction Refs.nth _ `Apply` _ `Apply` _ `Apply` bnd

evictOrphanedDestructured :: Expression -> Expression
evictOrphanedDestructured e = 
    let allBnds = HS.fromList [v | Let v _ _ <- universe e]
    in transform (f allBnds) e
    where 
        f bnds (Let v (NthFunction bnd) cont) | not $ HS.member bnd bnds = cont
        f _ e = e

findDestructured :: Expression -> Binding -> [Binding]
findDestructured e bnd = 
    let nths = sort 
                [ (i,v) 
                | (Let v 
                    (PureFunction Refs.nth _) `Apply` 
                    (Lit (NumericLit i)) `Apply` 
                    _ `Apply` 
                    bnd) 
                <- universe e]
    in nthts
applyToBody :: (Expression -> Expression) -> Expression -> Expression
applyToBody f e@(Lambda _ body) = applyToBody f body
applyToBody f e = f e

collectStates :: Expression -> [Binding]
collectStates e = HS.fromList [ state | (BindState (Var state) _) <- universe e ] 

mkST :: (MonadGenBnd m) => Expression -> m Expression
mkST states = \case
    Let v exp res -> return $ Let v exp $ mkST res
    e -> do
        allOut <- generateBindingWith "all_out" 
        return $
            Let allOut
                (mkApply ctxtExitFunRef $ e : states)
                (Var allOut)
