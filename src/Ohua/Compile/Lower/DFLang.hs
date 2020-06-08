module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.DFLang.Util
import Ohua.DFGraph
import Ohua.Backend.Lang
import Ohua.Backend.Types

import Data.Maybe
import Control.Monad.Extra (maybeM)


genCode :: CompM m => DFExpr -> m TCExpr
genCode graph = do
    -- TODO generate code for Ohua ops (ctrls, recurs, nths) if necessary
    nodesCode <- generateNodesCode graph
    let arcsCode = generateArcsCode graph
    resultReceive <- generateResultArc graph
    return $ 
        arcsCode $ 
            Run nodesCode resultReceive

generateNodesCode :: CompM m => DFExpr -> m [Task TCExpr]
generateNodesCode graph = toList <$> mapM generateNodeCode (letExprs graph)
    where 
        generateNodeCode :: CompM m => LetExpr -> m (Task TCExpr)
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
            let receiveCode cont = foldr (\(v,r) c -> Let v r c) cont varsAndReceives
            let stateVar = Var "state"
            stateReceiveCode <- 
                maybeM 
                    (return id) 
                    (\s -> do
                        -- assumes that state is used only in a single location!
                        -- is this really always the case? 
                        stateDFVar <- getStateVar s
                        idx <- getIndex stateDFVar e
                        return $ Let stateVar $ Receive idx $ Var $ unwrap stateDFVar)
                    $ return $ stateArgument e 
            fnCallCode <- 
                maybeM 
                    (return Stateless) 
                    (\_ -> return $ Stateful stateVar) 
                    $ return $ stateArgument e
            let callCode = Apply $ fnCallCode
                                    (nodeRef $ functionRef e) 
                                    $ map (Binding . fst) varsAndReceives

            let resultVar = Var "result"
            -- TODO do we support multiple outputs or is this here because of historical
            --      reasons? (It previously was meant for destructuring.)
            sendCode <- case output e of
                            [] -> return $ Lit UnitLit
                            [x] -> return $ Send (Var $ unwrap x) resultVar
                            _ -> throwError "Unsupported: multiple outputs detected."
            return $
                Task $
                    stateReceiveCode $ -- FIXME I don't think this is correct! 
                                       -- It needs to be just a more sophisticated arc that maintains the state instance for 'n' calls. (Control arc)
                        Loop $
                            receiveCode $
                                Let resultVar callCode sendCode

        getStateVar :: CompM m => DFVar -> m Binding
        getStateVar (DFVar bnd) = return bnd
        getStateVar (DFEnvVar _) = throwError "Invariant broken: state arg can not be literal!"

        convertDFVar :: DFVar -> TCExpr
        convertDFVar (DFVar bnd) = Binding $ Var $ unwrap bnd
        convertDFVar (DFEnvVar l) = Lit l 

        generateReceiveCode :: CompM m => DFVar -> Int -> LetExpr -> m (Var, TCExpr)
        generateReceiveCode (DFVar bnd) callIdx current = do
            idx <- getIndex bnd current
            return (Var $ "x" <> show callIdx, Receive idx $ Var $ unwrap bnd)
        generateReceiveCode (DFEnvVar l) callIdx _ = return (Var $ "x" <> show callIdx, Lit l)

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

generateArcsCode :: DFExpr -> TCExpr -> TCExpr
generateArcsCode graph cont = 
    foldr (\e c -> e c) cont $
    concat $ 
    flip map (letExprs graph) $ \letExpr ->
        flip map (output letExpr) $ \out ->
            let numUsages = length $ findUsages out $ letExprs graph
            in Let (Var $ unwrap out) (Channel numUsages)

generateResultArc :: CompM m => DFExpr -> m TCExpr
generateResultArc graph = 
    let retVar = returnVar graph
    in case length $ findUsages retVar $ letExprs graph of
        0 -> return $ Receive 0 $ Var $ unwrap retVar 
        _ -> throwError "Unsupported: use of final result elsewhere in the code."
