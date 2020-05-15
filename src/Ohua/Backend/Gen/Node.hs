module Ohua.Backend.Gen.Node where

import Ohua.DFGraph
import Ohua.Backend.DAGLang as DL

generateNodesCode :: CompM m => DFExpr -> m TCExpr
generateNodesCode graph = TList <$> (mapM generateNodeCode) . letExprs
    where 
        generateNodeCode e@(LetExpr {functionRef=DFFnRef{nodeType=OperatorNode}}) = 
            Task $ 
                Loop $ 
                    Apply 
                        (nodeRef $ functionRef e)
                        $ map convertDFVar $ callArguments e

        generateNodeCode e@(LetExpr {functionRef=DFFnRef{nodeType=FunctionNode}}) = do
            varsAndReceives <- mapM (generateReceiveCode e) $ callArguments e
            let receiveCode = \cont -> foldr (\(v,r) c -> Let v r c) cont varsAndReceives
            stateReceiveCode <- 
                maybe 
                    (return id) 
                    ((\v -> Receive (getIndex v e) $ Var v) <$> getStateVar) -- assumes that state is used only in a single location!
                                                -- is this really always the case? 
                    $ stateArgument e 
            fnCallCode <- maybe (return id) (State <$> getStateVar) $ stateArgument e
            let callCode = Apply $ fnCallCode
                                    (nodeRef $ functionRef e) 
                                    $ map (Left .fst) varsAndReceives

            let resultVar = Var "result"
            -- TODO do we support multiple outputs or is this here because of historical
            --      reasons? (It previously was meant for destructuring.)
            let sendCode = Send (Var $ head $ output e) resultVar
            return $
                Task $
                    stateReceiveCode $ -- FIXME I don't think this is correct! 
                                       -- It needs to be just a more sophisticated arc that maintains the state instance for 'n' calls. (Control arc)
                    Loop $
                        receiveCode $
                            Let resultVar (stateCallCode callCode)
                                sendCode

        getStateVar (DFVar bnd) = return bnd
        getStateVar (DFEnvVar _) = throwError "Invariant broken: state arg can not be literal!"

        convertDFVar (DFVar bnd) = Left bnd
        convertDFVar (DFEnvVar l) = Right l 

        generateReceiveCode (DFVar bnd) current = do
            idx <- getIndex bnd current
            return (Var $ "x" <> show idx, Receive idx $ Var bnd)
        generateReceiveCode (DFEnvVar l) _ = (Var $ "x" <> show idx, Lit l)

        getIndex bnd current = 
            let usages = findUsages bnd graph
            let indexed = zip usages [0 ..]
            let expr = find ((== (functionRef current)) . functionRef . fst) indexed
            return $ case expr of
                        Just e  -> snd e
                        -- This error could be avoided if we started of to construct the code
                        -- directly from the vars. But it feels that this algo would need a lot
                        -- of intermediate data structures.
                        Nothing -> throwError "Graph inconsistency: Can't find my usage of DFVar!"
