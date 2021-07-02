module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude
import qualified Ohua.Types.Vector as V

import Ohua.Core.DFLang.Lang as DFLang
import Ohua.Core.DFLang.Refs as Refs
import Ohua.Backend.Lang as BLang
import Ohua.Backend.Types
import qualified Ohua.Backend.Operators as Ops
import Ohua.Backend.Fusion as Fusion

import qualified Data.List.NonEmpty as NE ((<|), toList)
import qualified Data.HashSet as HS


-- Invariant in the result type: the result channel is already part of the list of channels.
toTCLang :: CompM m => NormalizedDFExpr ty -> m (TCProgram (Channel ty) (Com 'Recv ty) (FusableExpr ty))
toTCLang gr = do
    let channels = generateArcsCode gr
    (tasks, resultChan) <- generateNodesCode gr
    return $ TCProgram channels resultChan tasks

type LoweringM m a = m a

invariantBroken :: CompM m => Text -> LoweringM m a
invariantBroken msg = throwError $ "Compiler invariant broken! " <> msg

generateNodesCode :: CompM m => NormalizedDFExpr ty ->  LoweringM m ([FusableExpr ty], Com 'Recv ty)
generateNodesCode = go
    where
        go (DFLang.Let app cont) = do
            task <- generateNodeCode app
            (tasks, resRecv) <- go cont
            return (task:tasks,resRecv)
        go (DFLang.Var bnd) = return ([], SRecv TypeVar $ SChan bnd) -- FIXME needs a concrete type!

generateFunctionCode :: CompM m => DFApp a ty ->  LoweringM m (FusableExpr ty)
generateFunctionCode = \case
    (PureDFFun out fn inp) -> do
        (fun', args) <- lowerFnRef fn inp
        out' <- pureOut fn out
        return $ Fusion.Fun $ Ops.PureFusable args fun' $ Identity $ SChan out'
    (StateDFFun out fn (DFVar stateT stateIn) inp) -> do
        (fun', args) <- lowerFnRef fn inp
        (sOut, dataOut) <- stateOut fn out
        return $
          Fusion.Fun $
          Ops.STFusable
           (SRecv stateT $ SChan $ unwrapABnd stateIn)
           args
           fun'
           (SChan <$> dataOut) (SChan <$> sOut)
    where
        pureOut _ (Direct out) = return $ unwrapABnd out
        pureOut fn e = throwError $ "Unsupported multiple outputs on pure function " <> show fn <> ": " <> show e
        stateOut fn (sOut, out) = do
          sOut' <- toDirect fn sOut
          out' <- toDirect fn out
          return (sOut', out')
        toDirect _ (Just (Direct bnd)) = return $ Just $ unwrapABnd bnd
        toDirect _ Nothing = return Nothing
        toDirect fn e = throwError $ "Unsupported multiple outputs on stateful function " <> show fn <> ": " <> show e

generateReceive :: DFVar semTy ty -> Ops.CallArg ty
generateReceive (DFVar t bnd) =
    Ops.Arg $ SRecv t $ SChan $ unwrapABnd bnd
generateReceive (DFEnvVar t l) = Ops.Converted $ Lit l -- FIXME looses type info!

lowerFnRef :: CompM m
           => QualifiedBinding
           -> NonEmpty (DFVar semTy ty)
           -> LoweringM m (QualifiedBinding, [Ops.CallArg ty])
-- lowerFnRef fun vars | fun == Refs.unitFun = do -- FIXME Why not give a unit function a unit literal as an input?!
--     (f,vars') <-
--       case vars of
--         [DFEnvVar _t (FunRefLit (FunRef p _ _)), DFVar t1 bnd] -> -- FIXME looses type info!
--           return (p, [Ops.Drop $ Left $ SRecv t1 $ SChan $ unwrapABnd bnd])
--         [DFEnvVar _t (FunRefLit (FunRef p _ _)), DFEnvVar _ UnitLit] ->
--           return (p, [])
--         _ -> invariantBroken "unitFun must always have two arguments!"
--     return (f, vars')
lowerFnRef f [DFEnvVar _ UnitLit] = return (f, [])
lowerFnRef f vars = return (f, toList $ map generateReceive vars)

generateArcsCode :: NormalizedDFExpr ty -> NonEmpty (Channel ty)
generateArcsCode = go
    where
        go (DFLang.Let app cont) =
            let collected = go cont
                collected' = HS.fromList $ NE.toList collected
                current = filter (not . (`HS.member` collected')) $ map SChan $ inBindings app
            in foldl (flip (NE.<|)) collected current
        go (DFLang.Var bnd) = SChan bnd :|[]

-- FIXME see sertel/ohua-core#7: all these errors would immediately go away
generateNodeCode :: CompM m => DFApp semTy ty ->  LoweringM m (FusableExpr ty)
generateNodeCode e@(PureDFFun out fun inp)  | fun == smapFun = do
    input <-
        case inp of
            [x] -> case x of
                        (DFVar t v) -> return $ SRecv t $ SChan $ unwrapABnd v
                        _ -> invariantBroken $ "Input to SMap must var not literal:\n" <> show e
            _ -> invariantBroken $ "SMap should only have a single input." <> show e
    (dataOut, ctrlOut, collectOut) <-
        case out of -- FIXME: I can not even be sure here whether the order is the correct one!
            Destruct [Direct x, Direct y, Direct z] ->
                return (SChan $ unwrapABnd x, SChan $ unwrapABnd y, SChan $ unwrapABnd z)
            _ -> invariantBroken $ "SMap must have 3 outputs:\n" <> show e
    return $ SMap $ Ops.smapFun input dataOut ctrlOut collectOut

generateNodeCode e@(PureDFFun out fun inp) | fun == collect = do
    (sizeIn, dataIn) <-
        case inp of
            [DFVar sType s, DFVar dType d] ->
                return (SRecv sType $ SChan $ unwrapABnd s, SRecv dType $ SChan $ unwrapABnd d)
            _ -> invariantBroken $ "Collect arguments don't match:\n" <> show e
    collectedOutput <-
        case out of
            Direct x -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "Collect outputs don't match:\n" <> show e
    return $ SMap $ Ops.collect dataIn sizeIn collectedOutput

generateNodeCode e@(PureDFFun out fun inp) | fun == ifFun = do
    condIn <-
        case inp of
            [DFVar xType x] -> return $ SRecv xType $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "IfFun arguments don't match:\n" <> show e
    (ctrlTrueOut, ctrlFalseOut) <-
        case out of
            Destruct [Direct t, Direct fa] -> return (SChan $ unwrapABnd t, SChan $ unwrapABnd fa)
            _ -> invariantBroken $ "IfFun outputs don't match:\n" <> show e
    return $ Unfusable $
        EndlessLoop $
            Ops.ifFun condIn ctrlTrueOut ctrlFalseOut

generateNodeCode e@(PureDFFun out fun inp) | fun == select = do
    (condIn, trueIn, falseIn) <-
        case inp of
            [DFVar xType x, DFVar yType y, DFVar zType z] ->
                return
                    ( SRecv xType $ SChan $ unwrapABnd x
                    , SRecv yType $ SChan $ unwrapABnd y
                    , SRecv zType $ SChan $ unwrapABnd z)
            _ -> invariantBroken $ "Select arguments don't match:\n" <> show e
    out' <-
        case out of
            (Direct bnd) -> return $ SChan $ unwrapABnd bnd
            _ -> invariantBroken $ "Select outputs don't match:\n" <> show e
    return $ Unfusable $
        EndlessLoop $
            Ops.select condIn trueIn falseIn out'

generateNodeCode e@(PureDFFun out fun inp) | fun == Refs.runSTCLangSMap = do
    (sizeIn, stateIn) <-
        case inp of
            [DFVar xType x, DFVar yType y] ->
                return
                    ( SRecv xType $ SChan $ unwrapABnd x
                    , SRecv yType $ SChan $ unwrapABnd y)
            _ -> invariantBroken $ "STCLangSMap arguments don't match:\n" <> show e
    out' <-
        case out of
            Direct x -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "STCLangSMap outputs don't match:\n" <> show e
    return $ STC $
            Ops.mkSTCLangSMap
                sizeIn
                stateIn
                out'

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
            Direct x -> return $ SChan $ unwrapABnd x
            Destruct [Direct x] -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "Control outputs don't match:\n" <> show e
    case inp of
        DFVar tc ctrlInp :| [DFVar ti inp'] ->
            return $ Control $ Left $
                Ops.mkCtrl
                    (SRecv tc $ SChan $ unwrapABnd ctrlInp)
                    (SRecv ti $ SChan $ unwrapABnd inp')
                    out'
        DFVar tc ctrlInp :| [DFEnvVar _ti lit] ->
            return $ Control $ Right $
                Ops.mkLittedCtrl (SRecv tc $ SChan $ unwrapABnd ctrlInp) lit out' -- FIXME loosing the semantic type here!
        _ -> invariantBroken $ "Control arguments don't match:\n" <> show e

generateNodeCode e@(PureDFFun out fun inp) | fun == Refs.seqFun = do
  out' <- case out of
           Direct x -> return $ SChan $ unwrapABnd x
           _ -> invariantBroken $ "Seq must only have one output:\n" <> show e
  case inp of
    DFVar t1 inpVar :| [DFEnvVar _ l] ->
      return $
        Unfusable $
        Stmt (ReceiveData $ SRecv t1 $ SChan $ unwrapABnd inpVar) $
        BLang.Let "x" (Lit l) $
        SendData $ SSend out' "x"
    _ -> invariantBroken $
            "Seq must have two inputs where the second is a literal:\n" <> show e

generateNodeCode e@(PureDFFun out fun inp) | fun == Refs.unitFun = do
  out' <- case out of
           Direct x -> return $ SChan $ unwrapABnd x
           _ -> invariantBroken $ "unitFun must only have one output:\n" <> show e
  case inp of
   [DFEnvVar _t (FunRefLit (FunRef p _ _)), v] | p == Refs.id ->
     case v of
        (DFVar t bnd) ->
          return $
            Fusion.Fun $
            Ops.IdFusable (Ops.Arg $ SRecv t $ SChan $ unwrapABnd bnd) $ Identity out'
        (DFEnvVar _ l) ->
          return $
            Fusion.Fun $
            Ops.IdFusable (Ops.Converted $ Lit l) $ Identity out'
   [DFEnvVar _t (FunRefLit (FunRef p _ _)), v] -> -- FIXME this feels like a bug to me. why do we take this detour via unitFun???
     generateFunctionCode $ PureDFFun out p (v:|[])
   _ -> invariantBroken $ "unknown function as first argument or wrong number of arguments (expetced 2) to unitFun:\n" <> show e

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

generateNodeCode e@(RecurFun resultOut ctrlOut recArgsOuts recInitArgsIns recArgsIns recCondIn recResultIn) = do
    resultOut' <- directOut resultOut
    ctrlOut' <- directOut ctrlOut
    recArgsOuts' <- mapM directOut $ V.toList recArgsOuts
    let recInitArgsIns' = map varToChanOrLit $ V.toList recInitArgsIns
    -- TODO we could allow literals in the recursion args as well but we should require one to be a variable that got computed!
    recArgsIns' <- mapM varToChan $ V.toList recArgsIns
    recCondIn' <- varToChan recCondIn -- not allowing literals here to prevent at some form of infinite loop
    -- note that this aspect is interesting:
    -- normally, I could just say that the result of a recursion may just be a literal.
    -- but then what would that recursion actually compute?!
    -- looking at it from a timing-perspective, a recursion can as such be used to produce a delay in the computation.
    recResultIn' <- varToChan recResultIn
    return $ Recur
            $ Ops.RecFun
                resultOut' ctrlOut' recArgsOuts'
                recInitArgsIns' recArgsIns' recCondIn' recResultIn'
    where
        -- stronger typing needed on OutData to prevent this error handling here.
        -- (as a matter of fact it might be possible for some output to be destructured etc.
        --  we need a function here that turns such a thing into the appropriate backend code!)
        directOut :: CompM m => OutData a -> LoweringM m (Com 'Channel ty)
        directOut x = case x of
                        Direct x' -> return $ SChan $ unwrapABnd x'
                        _ -> invariantBroken $ "Control outputs don't match:\n" <> show e
        varToChanOrLit :: DFVar a ty -> Either (Com 'Recv ty) (Lit ty)
        varToChanOrLit (DFVar t v) = Left $ SRecv t $ SChan $ unwrapABnd v
        varToChanOrLit (DFEnvVar _ l) = Right l
        varToChan (DFVar t v) = return $ SRecv t $ SChan $ unwrapABnd v
        -- FIXME the below case needs to be checked during checking the well-formedness of the recursion and then carried along properly in the type.
        varToChan v = invariantBroken $ "environment variable not allowed in this position for recursion: " <> show v
generateNodeCode e = generateFunctionCode e

