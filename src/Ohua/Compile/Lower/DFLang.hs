module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude

import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.DFLang.Util
import Ohua.Core.DFGraph
import Ohua.Backend.Lang
import Ohua.Backend.Types
import qualified Ohua.Backend.Operators as Ops

import Data.Maybe
import qualified Data.HashSet as HS
import Control.Monad.Extra (maybeM)


-- Invariant in the result type: the result channel is already part of the list of channels.
toTCLang :: CompM m => DFExpr -> m (TCProgram Channel TaskExpr)
toTCLang graph = runGenBndT taken $ runReaderT transform graph
    where 
        transform = do
            let chans = generateArcsCode graph
            let resultChan = generateResultArc graph
            tasks <- generateNodesCode graph
            return $ TCProgram chans resultChan tasks
        taken = definedBindings graph
        definedBindings :: DFExpr -> HS.HashSet Binding
        definedBindings e =
            HS.fromList $ returnVar e : concatMap output (letExprs e)

type LoweringM m a = ReaderT DFExpr (GenBndT m) a

invariantBroken :: CompM m => Text -> LoweringM m a
invariantBroken msg = lift $ lift $ throwError $ "Compiler invariant broken! " <> msg

generateNodesCode :: CompM m => DFExpr ->  LoweringM m [TaskExpr]
generateNodesCode graph = toList <$> mapM generateNodeCode (letExprs graph)

generateFunctionCode :: CompM m => LetExpr ->  LoweringM m TaskExpr
generateFunctionCode e@LetExpr {functionRef=DFFnRef{nodeType=OperatorNode}} = 
    invariantBroken "`generateOpCode` should take care of all operators!"
generateFunctionCode e@LetExpr {functionRef=DFFnRef{nodeType=FunctionNode}} = do
    varsAndReceives <- mapM
                        (\(idx, arg) -> generateReceiveCode arg idx e)
                        $ zip [0..] $ callArguments e
    let loopCode varsAndReceives cont = foldr (\(v,r) c -> Let v r c) cont varsAndReceives
    stateVar <- lift $ generateBindingWith "state"
    stateReceiveCode <- 
        maybeM 
            (return id) 
            (\s -> do
                -- assumes that state is used only in a single location!
                -- is this really always the case? 
                stateDFVar <- getStateVar s
                idx <- getIndex stateDFVar e
                return $ Let stateVar $ Receive idx stateDFVar)
            $ return $ stateArgument e 
    fnCallCode <- 
        maybeM 
            (return Stateless) 
            (\_ -> return $ Stateful stateVar) 
            $ return $ stateArgument e
    
    (varsAndReceives', fun, args) <- lowerFnRef (functionRef e) varsAndReceives

    let callCode = Apply $ fnCallCode fun args

    resultVar <- generateBindingWith "result"
    -- TODO do we support multiple outputs or is this here because of historical
    --      reasons? (It previously was meant for destructuring.)
    sendCode <- case output e of
                    [] -> return $ Lit UnitLit
                    [x] -> return $ Send x resultVar
                    _ -> lift $ lift $ throwError "Unsupported: multiple outputs detected."
    return $ stateReceiveCode $ -- FIXME I don't think this is correct! 
                                -- It needs to be just a more sophisticated arc that maintains the state instance for 'n' calls. (Control arc)
                EndlessLoop $
                    loopCode 
                        varsAndReceives'
                        (Let resultVar callCode sendCode)

getStateVar :: CompM m => DFVar -> LoweringM m Binding
getStateVar (DFVar bnd) = return bnd
getStateVar v = invariantBroken $ "State arg can not be literal! " <> show v

convertDFVar :: DFVar -> TaskExpr
convertDFVar (DFVar bnd) = Var bnd
convertDFVar (DFEnvVar l) = Lit l 

generateReceiveCode :: CompM m => DFVar -> Int -> LetExpr -> LoweringM m (Binding, TaskExpr)
generateReceiveCode (DFVar bnd) callIdx current = do
    idx <- getIndex bnd current
    x <- generateBindingWith $ "arg" <> show callIdx
    return (x, Receive idx bnd)
generateReceiveCode (DFEnvVar l) callIdx _ = do
    x <- generateBindingWith $ "arg" <> show callIdx
    return (x, Lit l)

getIndex :: CompM m => Binding -> LetExpr -> LoweringM m Int
getIndex bnd current = do
    usages <- findUsages bnd . letExprs <$> ask
    let indexed = zip usages [0 ..]
    let expr = find ((== functionRef current) . functionRef . fst) indexed
    case expr of
        Just e  -> return $ snd e
        -- This error could be avoided if we started of to construct the code
        -- directly from the vars. But it feels that this algo would need a lot
        -- of intermediate data structures.
        Nothing -> invariantBroken "Graph inconsistency: Can't find my usage of DFVar!"

lowerFnRef :: CompM m => DFFnRef -> [(Binding, TaskExpr)] -> LoweringM m ([(Binding, TaskExpr)], QualifiedBinding, [TaskExpr])
lowerFnRef fun varsAndReceives | fun == Refs.unitFun = do
    f <- case nonEmpty varsAndReceives of
            Just vs -> case snd $ head vs of
                            Lit (FunRefLit (FunRef p _)) -> return p
                            _ -> invariantBroken "unitFun must always have a function as its first argument!"
            Nothing -> invariantBroken "unitFun must always have two arguments!"
    return ([], f, [])
lowerFnRef f varsAndReceives = 
    return (varsAndReceives, nodeRef f, map (Var . fst) varsAndReceives)

generateArcsCode :: DFExpr -> [Channel]
generateArcsCode graph =
    concat $ 
    flip map (letExprs graph) $ \letExpr ->
        flip map (output letExpr) $ \out ->
            let numUsages = length $ findUsages out $ letExprs graph
            in Channel out numUsages

generateResultArc :: DFExpr -> Channel
generateResultArc = flip Channel 1 . returnVar

-- FIXME see sertel/ohua-core#7: all these errors would immediately go away
generateNodeCode :: CompM m => LetExpr ->  LoweringM m TaskExpr
generateNodeCode e@LetExpr {functionRef=f} | f == smapFun = do
    input <- 
        case callArguments e of
            [x] -> case x of 
                        (DFVar v) -> return v
                        _ -> invariantBroken $ "Input to SMap must var not literal: " <> show e
            _ -> invariantBroken $ "SMap should only have a single input." <> show e
    (dataOut, ctrlOut, collectOut) <- 
        case output e of -- FIXME: I can not even be sure here whether the order is the correct one!
            [x,y,z] -> return (x,y,z)
            _ -> invariantBroken $ "SMap must have 3 outputs: " <> show e
    lift $ return $
        EndlessLoop $
            Ops.smapFun input dataOut ctrlOut collectOut

generateNodeCode e@LetExpr {functionRef=f} | f == collect = do
    (sizeIn, dataIn, stateIn, collectFun) <-
        case callArguments e of
            [DFVar s, DFVar d, DFVar st, DFEnvVar (FunRefLit f)] -> return (s,d,st,f)
            _ -> invariantBroken $ "Collect arguments don't match: " <> show e
    collectedOutput <- 
        case output e of
            [x] -> return x
            _ -> invariantBroken $ "Collect outputs don't match" <> show e
    lift $ return $
        EndlessLoop $
            Ops.collect sizeIn dataIn stateIn collectFun collectedOutput

generateNodeCode e@LetExpr {functionRef=f} | f == ifFun= do
    condIn <- 
        case callArguments e of
            [DFVar x] -> return x
            _ -> invariantBroken $ "IfFun arguments don't match: " <> show e
    (ctrlTrueOut, ctrlFalseOut) <-
        case output e of
            [t, f] -> return (t,f)
            _ -> invariantBroken $ "IfFun outputs don't match" <> show e
    lift $ return $
        EndlessLoop $
            Ops.ifFun condIn ctrlTrueOut ctrlFalseOut

generateNodeCode e@LetExpr {functionRef=f} | f == select = do
    (condIn, trueIn, falseIn) <- 
        case callArguments e of
            [DFVar x, DFVar y, DFVar z] -> return (x,y,z)
            _ -> invariantBroken $ "Select arguments don't match: " <> show e
    out <-
        case output e of
            [x] -> return x
            _ -> invariantBroken $ "Select outputs don't match" <> show e
    lift $ return $
        EndlessLoop $
            Ops.select condIn trueIn falseIn out

generateNodeCode e@LetExpr {functionRef=f} | f == ctrl = do
    -- invariants: len ins == len outs, NonEmpty ins, NonEmpty outs
    (ctrlIn, ins) <- 
        case callArguments e of
            DFVar c:is -> 
                (c,) <$> forM is (\case 
                                    DFVar v -> return v
                                    DFEnvVar _ -> invariantBroken $ "Control argument can not be literal: " <> show e)
            _ -> invariantBroken $ "Control arguments don't match: " <> show e
    outs <-
        case output e of
            [] -> invariantBroken $ "Control outputs don't match" <> show e    
            xs -> return xs
    lift $ return $
        EndlessLoop $
            Ops.ctrl ctrlIn ins outs

generateNodeCode e@LetExpr {functionRef=f} | f == recurFun = undefined
generateNodeCode e = generateFunctionCode e
    
