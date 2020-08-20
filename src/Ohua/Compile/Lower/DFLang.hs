module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude

import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.DFLang.Util
import Ohua.Core.DFGraph
import Ohua.Backend.Lang
import Ohua.Backend.Types

import Data.Maybe
import qualified Data.HashSet as HS
import Control.Monad.Extra (maybeM)


toTCLang :: CompM m => DFExpr -> m TCExpr
toTCLang graph = runGenBndT taken transform
    where 
        transform = do
            -- TODO generate code for Ohua ops (ctrls, recurs, nths) if necessary
            nodesCode <- generateNodesCode graph
            let arcsCode = generateArcsCode graph
            resultReceive <- lift $ generateResultArc graph
            return $
                arcsCode $
                    Run nodesCode resultReceive
        
        taken = definedBindings graph
        definedBindings :: DFExpr -> HS.HashSet Binding
        definedBindings e =
            HS.fromList $ returnVar e : concatMap output (letExprs e)


generateNodesCode :: CompM m => DFExpr ->  GenBndT m [Task TCExpr]
generateNodesCode graph = toList <$> mapM generateNodeCode (letExprs graph)
    where 
        generateNodeCode :: CompM m => LetExpr ->  GenBndT m (Task TCExpr)
        generateNodeCode e@LetExpr {functionRef=DFFnRef{nodeType=OperatorNode}} = 
            return $
                Task $ 
                    Loop $ 
                        Apply $ 
                            Stateless
                                (nodeRef $ functionRef e)
                                $ map convertDFVar $ callArguments e

        generateNodeCode e@LetExpr {functionRef=DFFnRef{nodeType=FunctionNode}} = do
            varsAndReceives <- mapM
                                (\(idx, arg) -> generateReceiveCode arg idx e)
                                $ zip [0..] $ callArguments e
            let loopCode varsAndReceives cont = foldr (\(v,r) c -> Let v r c) cont varsAndReceives
            stateVar <- generateBindingWith "state"
            stateReceiveCode <- 
                maybeM 
                    (return id) 
                    (\s -> do
                        -- assumes that state is used only in a single location!
                        -- is this really always the case? 
                        stateDFVar <- lift$ getStateVar s
                        idx <- lift $ getIndex stateDFVar e
                        return $ Let stateVar $ Receive idx stateDFVar)
                    $ return $ stateArgument e 
            fnCallCode <- 
                maybeM 
                    (return Stateless) 
                    (\_ -> return $ Stateful stateVar) 
                    $ return $ stateArgument e
            
            (varsAndReceives', fun, args) <- lift $ lowerFnRef (functionRef e) varsAndReceives

            let callCode = Apply $ fnCallCode fun args

            resultVar <- generateBindingWith "result"
            -- TODO do we support multiple outputs or is this here because of historical
            --      reasons? (It previously was meant for destructuring.)
            sendCode <- case output e of
                            [] -> return $ Lit UnitLit
                            [x] -> return $ Send x resultVar
                            _ -> lift $ throwError "Unsupported: multiple outputs detected."
            return $
                Task $
                    stateReceiveCode $ -- FIXME I don't think this is correct! 
                                       -- It needs to be just a more sophisticated arc that maintains the state instance for 'n' calls. (Control arc)
                        Loop $
                            loopCode 
                                varsAndReceives'
                                (Let resultVar callCode sendCode)

        getStateVar :: CompM m => DFVar -> m Binding
        getStateVar (DFVar bnd) = return bnd
        getStateVar (DFEnvVar _) = throwError "Invariant broken: state arg can not be literal!"

        convertDFVar :: DFVar -> TCExpr
        convertDFVar (DFVar bnd) = Var bnd
        convertDFVar (DFEnvVar l) = Lit l 

        generateReceiveCode :: CompM m => DFVar -> Int -> LetExpr -> GenBndT m (Binding, TCExpr)
        generateReceiveCode (DFVar bnd) callIdx current = do
            idx <- lift $ getIndex bnd current
            x <- generateBindingWith $ "arg" <> show callIdx
            return (x, Receive idx bnd)
        generateReceiveCode (DFEnvVar l) callIdx _ = do
            x <- generateBindingWith $ "arg" <> show callIdx
            return (x, Lit l)

        getIndex :: CompM m => Binding -> LetExpr -> m Int
        getIndex bnd current = 
            let usages = findUsages bnd $ letExprs graph
                indexed = zip usages [0 ..]
                expr = find ((== functionRef current) . functionRef . fst) indexed
            in case expr of
                    Just e  -> return $ snd e
                    -- This error could be avoided if we started of to construct the code
                    -- directly from the vars. But it feels that this algo would need a lot
                    -- of intermediate data structures.
                    Nothing -> throwError "Graph inconsistency: Can't find my usage of DFVar!"
        
        lowerFnRef :: CompM m => DFFnRef -> [(Binding, TCExpr)] -> m ([(Binding, TCExpr)], QualifiedBinding, [TCExpr])
        lowerFnRef fun varsAndReceives | fun == Refs.unitFun = do
            f <- case nonEmpty varsAndReceives of
                    Just vs -> case snd $ head vs of
                                    Lit (FunRefLit (FunRef p _)) -> return p
                                    _ -> throwError "unitFun must always have a function as its first argument! This is an internal compiler error. Please report!"
                    Nothing -> throwError "unitFun must always have two arguments! This is an internal compiler error. Please report!"
            return ([], f, [])
        lowerFnRef f varsAndReceives = 
            return (varsAndReceives, nodeRef f, map (Var . fst) varsAndReceives)

generateArcsCode :: DFExpr -> TCExpr -> TCExpr
generateArcsCode graph cont = 
    foldr (\e c -> e c) cont $
    concat $ 
    flip map (letExprs graph) $ \letExpr ->
        flip map (output letExpr) $ \out ->
            let numUsages = length $ findUsages out $ letExprs graph
            in Let out (Channel numUsages)

generateResultArc :: CompM m => DFExpr -> m TCExpr
generateResultArc graph = 
    let retVar = returnVar graph
    in case length $ findUsages retVar $ letExprs graph of
        0 -> return $ Receive 0 retVar 
        _ -> throwError "Unsupported: use of final result elsewhere in the code."
