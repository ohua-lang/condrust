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

ctxtExit :: Expression
ctxtExit = Lit $ FunRefLit $ FunRef "ohua.lang/ctxtExit" Nothing

addCtxtExit :: MonadGenBnd m => Expression -> m Expression
addCtxtExit = f
    where
        f (PureFunction op i `Apply` trueBranch `Apply` falseBranch)
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
                    return $
                        PureFunction op i `Apply` trueBranch'' `Apply` falseBranch''
    
        f (PureFunction op i `Apply` expr `Apply` _)
            | op == Refs.smap =
                let states = collectStates expr
                in do
                    expr' <- mkST states expr
                    return $
                        PureFunction op i `Apply` expr'

        f = descend f

-- Assumptions: This function is applied to a normalized and SSAed expression.
transformCtxtExits :: Binding -> Expression -> Expression
transformCtxtExits = undefined
    where
        f (Let v (PureFunction op _ `Apply` _) cont)
            | op == Refs.smapFun = do
                -- add the context to the stacks
                modify $ HM.map $ 
                    maybe
                        (runSTCLangFun :| [])
                        (runSTCLangFun <| )
                descendM cont

        f (Let v (PureFunction op _ `Apply` _) cont)
            | op == Refs.collectFun = do
                -- evict the context from the stacks
                modify $ HM.map 
                    $ \case
                        Just ctxts -> NE.nonEmpty $ tail ctxts
                        Nothing -> error "invariant broken" -- FIXME encode this invariant!
                descendM cont

        f (Let v (PureFunction op _ `Apply` _) cont)
            | op == Refs.ifFun = do
                -- add the context to the stacks
                descendM cont

        f (Let v (PureFunction op _ `Apply` _) cont)
            | op == Refs.selectFun = do
                -- evict the context from the stacks
                descendM cont

        f (Let v (PureFunction op _ `Apply` ctrlSig `Apply` _) cont)
            | op == Refs.controlFun = do
                -- instantiate the context with the required control information
                modify $ HM.map $ 
                    maybe
                        (error "invariant broken") -- FIXME encode this invariant!
                        (\ctxts -> head ctxts `Apply` ctrlSig :| tail ctxts)
                descendM cont
        
        f (Let v _ cont) = 
            -- register a new variable with an empty stack
            modify (HM.insert bnd Nothing)

        f (Let v st@(BindState (Var state) e) cont)
            | state == currentState = do
                -- return the state and unfold the context
                x <- generateBinding
                let stateThread = Let x st $ mkDestructured [v,state] x

                -- TODO enforce this invariant: there can not be a state that
                --      was not defined!
                ctxtStack <- fromMaybe . HM.lookup state <$> get
                let ctxtUnfolded = foldl (\c ctxtExit -> c $ Let state (ctxtExit `Apply` Var state)) stateThread ctxtStack 

                -- evict the key
                modify $ HM.delete state

                return $ ctxtUnfolded cont


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
                (mkApply ctxtExit $ e : states)
                (Var allOut)
