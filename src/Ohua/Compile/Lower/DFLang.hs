module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude

import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.DFLang.Util
import Ohua.Backend.Lang
import Ohua.Backend.Types
import qualified Ohua.Backend.Operators as Ops
import Ohua.Backend.Fusion

import qualified Data.HashSet as HS


-- Invariant in the result type: the result channel is already part of the list of channels.
toTCLang :: CompM m => DFExpr -> m (TCProgram Channel FusableExpr)
toTCLang gr = runGenBndT taken $ runReaderT go gr
    where 
        go = do
            let chans = generateArcsCode gr
            let resultChan = generateResultArc gr
            tasks <- generateNodesCode gr
            return $ TCProgram chans resultChan tasks
        taken = definedBindings gr
        definedBindings :: DFExpr -> HS.HashSet Binding
        definedBindings e =
            HS.fromList $ returnVar e : concatMap output (letExprs e)

type LoweringM m a = ReaderT DFExpr (GenBndT m) a

invariantBroken :: CompM m => Text -> LoweringM m a
invariantBroken msg = lift $ lift $ throwError $ "Compiler invariant broken! " <> msg

generateNodesCode :: CompM m => DFExpr ->  LoweringM m [FusableExpr]
generateNodesCode gr = toList <$> mapM generateNodeCode (letExprs gr)

generateFunctionCode :: CompM m => LetExpr ->  LoweringM m FusableExpr
generateFunctionCode e@LetExpr {functionRef=DFFnRef{nodeType=OperatorNode}} = 
    invariantBroken $ "`generateNodeCode` should take care of all operators! Slipped through: " <> show e
generateFunctionCode e@LetExpr {functionRef=DFFnRef{nodeType=FunctionNode}} = do
    (fun, argReceives) <- lowerFnRef (functionRef e) $ callArguments e
    f <- case stateArgument e of
            Just s -> do
                (stateOut,resOut) <- case output e of 
                                        [] -> return (Nothing, Nothing)
                                        [st,r] -> return (Just st, Just r)
                                        _ -> lift $ lift $ throwError $ "Unsupported multiple outputs: " <> show e
                stateReceive <- getStateVar s

                return $
                    Ops.ST
                        fun
                        (Recv 0 stateReceive)
                        argReceives
                        stateOut
                        resOut
            Nothing -> do
                resOut <- case output e of 
                        [r] -> return r
                        _ -> lift $ lift $ throwError $ "Unsupported multiple outputs: " <> show e
                return $
                    Ops.Pure
                        fun
                        argReceives
                        resOut
    return $ Fun $ Ops.fun f

getStateVar :: CompM m => DFVar -> LoweringM m Binding
getStateVar (DFVar bnd) = return bnd
getStateVar v = invariantBroken $ "State arg can not be literal! " <> show v

convertDFVar :: DFVar -> TaskExpr
convertDFVar (DFVar bnd) = Var bnd
convertDFVar (DFEnvVar l) = Lit l 

-- generateReceive :: CompM m => DFVar -> LoweringM m (Either Recv TaskExpr)
generateReceive :: DFVar -> Ops.CallArg
generateReceive (DFVar bnd) = -- do
    -- idx <- getIndex bnd current
    Ops.Arg $ Recv 0 bnd
generateReceive (DFEnvVar l) =
    Ops.Converted $ Lit l

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

lowerFnRef :: CompM m => DFFnRef -> [DFVar] -> LoweringM m (QualifiedBinding, [Ops.CallArg])
lowerFnRef fun vars | fun == Refs.unitFun = do
    (f,vars') <- case vars of
            [DFEnvVar (FunRefLit (FunRef p _)), DFVar bnd] -> 
                return (p, [Ops.Drop $ Left $ Recv 0 bnd])
            [DFEnvVar (FunRefLit (FunRef p _)), DFEnvVar UnitLit] -> 
                return (p, [])
            _ -> invariantBroken "unitFun must always have two arguments!"
    return (f, vars')
lowerFnRef f vars = return (nodeRef f, map generateReceive vars)

generateArcsCode :: DFExpr -> [Channel]
generateArcsCode gr =
    concat $ 
    flip map (letExprs gr) $ \letExpr ->
        flip map (output letExpr) $ \out ->
            let numUsages = length $ findUsages out $ letExprs gr
            in Channel out numUsages

generateResultArc :: DFExpr -> Channel
generateResultArc = flip Channel 1 . returnVar

-- FIXME see sertel/ohua-core#7: all these errors would immediately go away
generateNodeCode :: CompM m => LetExpr ->  LoweringM m FusableExpr
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
    lift $ return $ Unfusable $
        EndlessLoop $
            Ops.smapFun input dataOut ctrlOut collectOut

generateNodeCode e@LetExpr {functionRef=f} | f == collect = do
    (sizeIn, dataIn) <-
        case callArguments e of
            [DFVar s, DFVar d] -> return (s,d)
            _ -> invariantBroken $ "Collect arguments don't match: " <> show e
    collectedOutput <- 
        case output e of
            [x] -> return x
            _ -> invariantBroken $ "Collect outputs don't match" <> show e
    lift $ return $ Unfusable $
        EndlessLoop $ 
            Ops.collect sizeIn dataIn collectedOutput

generateNodeCode e@LetExpr {functionRef=f} | f == ifFun = do
    condIn <- 
        case callArguments e of
            [DFVar x] -> return x
            _ -> invariantBroken $ "IfFun arguments don't match: " <> show e
    (ctrlTrueOut, ctrlFalseOut) <-
        case output e of
            [t, fa] -> return (t,fa)
            _ -> invariantBroken $ "IfFun outputs don't match" <> show e
    lift $ return $ Unfusable $
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
    lift $ return $ Unfusable $
        EndlessLoop $
            Ops.select condIn trueIn falseIn out

generateNodeCode e@LetExpr {functionRef=f} | f == Refs.runSTCLangSMap = do
    (sizeIn, stateIn) <- 
        case callArguments e of
            [DFVar x, DFVar y] -> return (x,y)
            _ -> invariantBroken $ "STCLangSMap arguments don't match: " <> show e
    out <-
        case output e of
            [x] -> return x
            _ -> invariantBroken $ "STCLangSMap outputs don't match" <> show e
    lift $ return $ STC $
            Ops.mkSTCLangSMap 
                sizeIn
                stateIn
                out

-- code for "non-fused" control handling without the passes on ALang
-- generateNodeCode e@LetExpr {functionRef=f} | f == ctrl = do
--     -- invariants: len ins == len outs, NonEmpty ins, NonEmpty outs
--     (ctrlIn, ins) <- 
--         case callArguments e of
--             DFVar c:is -> 
--                 (c,) <$> forM (NE.fromList is) (\case
--                                     DFVar v -> return $ Recv 0 v
--                                     DFEnvVar _ -> invariantBroken $ "Control argument can not be literal: " <> show e)
--             _ -> invariantBroken $ "Control arguments don't match: " <> show e
--     outs <-
--         case output e of
--             [] -> invariantBroken $ "Control outputs don't match" <> show e    
--             xs -> return $ NE.fromList xs
--     lift $ return $
--         Control $ Ops.mkCtrl 
--                     ctrlIn 
--                     ins
--                     out

generateNodeCode e@LetExpr {functionRef=f} | f == ctrl = do
    out <-
        case output e of
            [x] -> return x
            _ -> invariantBroken $ "Control outputs don't match" <> show e    
    case callArguments e of
        DFVar ctrlInp:[DFVar inp] ->
            lift $ return $ Control $ Left $ Ops.mkCtrl ctrlInp inp out
        DFVar ctrlInp:[DFEnvVar lit] ->
            lift $ return $ Control $ Right $ Ops.mkLittedCtrl ctrlInp lit out
        _ -> invariantBroken $ "Control arguments don't match: " <> show e

-- generateNodeCode e@LetExpr {functionRef=f} | f == runSTCLang = do
--     (sizeIn, dataIn, stateIn, collectFun) <-
--         case callArguments e of
--             [DFVar s, DFVar d, DFVar st, DFEnvVar (FunRefLit f)] -> return (s,d,st,f)
--             _ -> invariantBroken $ "runSTCLang arguments don't match: " <> show e
--     collectedOutput <- 
--         case output e of
--             [x] -> return x
--             _ -> invariantBroken $ "runSTCLang outputs don't match" <> show e
--     lift $ return $
--         EndlessLoop $
--             Ops.runSTCLang sizeIn dataIn stateIn collectFun collectedOutput

generateNodeCode LetExpr {functionRef=f} | f == recurFun = undefined
generateNodeCode e = generateFunctionCode e
    
