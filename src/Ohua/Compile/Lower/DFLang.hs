module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude

import Ohua.Core.DFLang.Lang as DFLang
import Ohua.Core.DFLang.Refs as Refs
import Ohua.Backend.Lang as BLang
import Ohua.Backend.Types
import qualified Ohua.Backend.Operators as Ops
import Ohua.Backend.Fusion as Fusion


-- Invariant in the result type: the result channel is already part of the list of channels.
toTCLang :: CompM m => NormalizedDFExpr -> m (TCProgram Channel FusableExpr)
toTCLang gr = do
    let channels = generateArcsCode gr
    (tasks,resultChan) <- generateNodesCode gr
    return $ TCProgram channels resultChan tasks

type LoweringM m a = m a

invariantBroken :: CompM m => Text -> LoweringM m a
invariantBroken msg = throwError $ "Compiler invariant broken! " <> msg

generateNodesCode :: CompM m => NormalizedDFExpr ->  LoweringM m ([FusableExpr],Channel)
generateNodesCode = go
    where 
        go (DFLang.Let app cont) = do
            task <- generateNodeCode app
            (tasks, resChan) <- go cont
            return (task:tasks,resChan)
        go (DFLang.Var bnd) = return ([], Channel bnd 1) 

generateFunctionCode :: CompM m => DFApp a ->  LoweringM m FusableExpr
generateFunctionCode = \case
    (PureDFFun out fn inp) -> do
        (fun', args) <- lowerFnRef fn inp
        out' <- pureOut out 
        return $ Fusion.Fun $ Ops.PureFusable args fun' out'
    (StateDFFun out fn stateIn inp) -> do
        (fun', args) <- lowerFnRef fn inp
        (sOut, dataOut) <- stateOut out 
        return $ Fusion.Fun
            $ Ops.STFusable (Recv 0 $ unwrapABnd stateIn) args fun' dataOut sOut
    where
        pureOut (Direct out) = return $ unwrapABnd out
        pureOut e = throwError $ "Unsupported multiple outputs: " <> show e
        stateOut (stateOut, Direct out) = return ((\(Direct bnd) -> unwrapABnd bnd) <$> stateOut, Just $ unwrapABnd out)
        stateOut e = throwError $ "Unsupported multiple outputs: " <> show e

convertDFVar :: DFVar -> TaskExpr
convertDFVar (DFVar bnd) = BLang.Var $ unwrapABnd bnd
convertDFVar (DFEnvVar l) = Lit l 

generateReceive :: DFVar -> Ops.CallArg
generateReceive (DFVar bnd) = Ops.Arg $ Recv 0 $ unwrapABnd bnd
generateReceive (DFEnvVar l) = Ops.Converted $ Lit l

lowerFnRef :: CompM m => QualifiedBinding -> NonEmpty DFVar -> LoweringM m (QualifiedBinding, [Ops.CallArg])
lowerFnRef fun vars | fun == Refs.unitFun = do -- FIXME Why not give a unit function a unit literal as an input?!
    (f,vars') <- case vars of
            [DFEnvVar (FunRefLit (FunRef p _)), DFVar bnd] -> 
                return (p, [Ops.Drop $ Left $ Recv 0 $ unwrapABnd bnd])
            [DFEnvVar (FunRefLit (FunRef p _)), DFEnvVar UnitLit] -> 
                return (p, [])
            _ -> invariantBroken "unitFun must always have two arguments!"
    return (f, vars')
lowerFnRef f vars = return (f, toList $ map generateReceive vars)

generateArcsCode :: NormalizedDFExpr -> [Channel]
generateArcsCode = go
    where
        go (DFLang.Let app cont) = map (`Channel` 1) (toList $ outBindings app) ++ go cont
        go (DFLang.Var _) = []

-- FIXME see sertel/ohua-core#7: all these errors would immediately go away
generateNodeCode :: CompM m => DFApp a ->  LoweringM m FusableExpr
generateNodeCode e@(PureDFFun out fun inp)  | fun == smapFun = do
    input <- 
        case inp of
            [x] -> case x of 
                        (DFVar v) -> return $ unwrapABnd v
                        _ -> invariantBroken $ "Input to SMap must var not literal:\n" <> show e
            _ -> invariantBroken $ "SMap should only have a single input." <> show e
    (dataOut, ctrlOut, collectOut) <- 
        case out of -- FIXME: I can not even be sure here whether the order is the correct one!
            Destruct [Direct x, Direct y, Direct z] -> return (unwrapABnd x, unwrapABnd y, unwrapABnd z)
            _ -> invariantBroken $ "SMap must have 3 outputs:\n" <> show e
    return $ Unfusable $
        EndlessLoop $
            Ops.smapFun input dataOut ctrlOut collectOut

generateNodeCode e@(PureDFFun out fun inp) | fun == collect = do
    (sizeIn, dataIn) <-
        case inp of
            [DFVar s, DFVar d] -> return (unwrapABnd s, unwrapABnd d)
            _ -> invariantBroken $ "Collect arguments don't match:\n" <> show e
    collectedOutput <- 
        case out of
            Direct x -> return $ unwrapABnd x
            _ -> invariantBroken $ "Collect outputs don't match:\n" <> show e
    return $ Unfusable $
        EndlessLoop $ 
            Ops.collect sizeIn dataIn collectedOutput

generateNodeCode e@(PureDFFun out fun inp) | fun == ifFun = do
    condIn <- 
        case inp of
            [DFVar x] -> return $ unwrapABnd x
            _ -> invariantBroken $ "IfFun arguments don't match:\n" <> show e
    (ctrlTrueOut, ctrlFalseOut) <-
        case out of
            Destruct [Direct t, Direct fa] -> return (unwrapABnd t, unwrapABnd fa)
            _ -> invariantBroken $ "IfFun outputs don't match:\n" <> show e
    return $ Unfusable $
        EndlessLoop $
            Ops.ifFun condIn ctrlTrueOut ctrlFalseOut

generateNodeCode e@(PureDFFun out fun inp) | fun == select = do
    (condIn, trueIn, falseIn) <- 
        case inp of
            [DFVar x, DFVar y, DFVar z] -> return (unwrapABnd x, unwrapABnd y, unwrapABnd z)
            _ -> invariantBroken $ "Select arguments don't match:\n" <> show e
    out <-
        case out of
            (Direct bnd) -> return $ unwrapABnd bnd
            _ -> invariantBroken $ "Select outputs don't match:\n" <> show e
    return $ Unfusable $
        EndlessLoop $
            Ops.select condIn trueIn falseIn out

generateNodeCode e@(PureDFFun out fun inp) | fun == Refs.runSTCLangSMap = do
    (sizeIn, stateIn) <- 
        case inp of
            [DFVar x, DFVar y] -> return (unwrapABnd x, unwrapABnd y)
            _ -> invariantBroken $ "STCLangSMap arguments don't match:\n" <> show e
    out <-
        case out of
            Direct x -> return $ unwrapABnd x
            _ -> invariantBroken $ "STCLangSMap outputs don't match:\n" <> show e
    return $ STC $
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

generateNodeCode e@(PureDFFun out fun inp) | fun == ctrl = do
    out' <-
        case out of
            Direct x -> return $ unwrapABnd x
            Destruct [Direct x] -> return $ unwrapABnd x
            _ -> invariantBroken $ "Control outputs don't match:\n" <> show e    
    case inp of
        DFVar ctrlInp :| [DFVar inp'] ->
            return $ Control $ Left $ 
                Ops.mkCtrl (unwrapABnd ctrlInp) (unwrapABnd inp') out'
        DFVar ctrlInp :| [DFEnvVar lit] ->
            return $ Control $ Right $ 
                Ops.mkLittedCtrl (unwrapABnd ctrlInp) lit out'
        _ -> invariantBroken $ "Control arguments don't match:\n" <> show e

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

generateNodeCode (PureDFFun _out fun _inp) | fun == recurFun = undefined
generateNodeCode e = generateFunctionCode e
    
