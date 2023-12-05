module Ohua.Compile.Lower.DFLang where

import Ohua.Prelude
import qualified Ohua.Types.Vector as V


import Ohua.Core.DFLang.Lang as DFLang
import Ohua.Core.InternalFunctions as IFuns 
import Ohua.Backend.Lang as BLang
import Ohua.Backend.Types
import qualified Ohua.Backend.Operators as Ops
import Ohua.Backend.Fusion as Fusion

import qualified Data.List.NonEmpty as NE (NonEmpty( (:|) ), (<|), toList, length, zip, map)
import qualified Data.HashSet as HS

import Ohua.Core.DFLang.PPrint (prettyExpr)

-- Invariant in the result type: the result channel is already part of the list of channels.
toTCLang :: (ErrAndLogM m, Show ty) => NormalizedDFExpr ty -> m (TCProgram (Channel ty) (Com 'Recv ty) (FusableExpr ty))
toTCLang gr = do
    let channels = generateArcsCode gr
    (tasks, resultChan) <- generateNodesCode gr
    return $ TCProgram channels resultChan tasks

type LoweringM m a = m a

invariantBroken :: ErrAndLogM m => Text -> LoweringM m a
invariantBroken msg = throwError $ "Compiler invariant broken! " <> msg

generateNodesCode :: (ErrAndLogM m, Show (FusableExpr ty)) => NormalizedDFExpr ty ->  LoweringM m ([FusableExpr ty], Com 'Recv ty)
generateNodesCode = go
    where
        go :: (ErrAndLogM m, Show (FusableExpr ty)) => NormalizedDFExpr ty -> LoweringM m ([FusableExpr ty], Com 'Recv ty)
        go (DFLang.Let app cont) = do
            task <- generateNodeCode app
            traceM $"Generated Task :\n" <> show task <> "\n"
            (tasks, resRecv) <- go cont
            return (task:tasks,resRecv)
        go (DFLang.Var atBnd) = 
          let (TBind bnd ty) = unwrapTB atBnd
          in return ([], SRecv ty $ SChan bnd) 

generateFunctionCode :: forall ty a m. ErrAndLogM m => DFApp a ty -> LoweringM m (FusableExpr ty)
generateFunctionCode = \case
    
    (PureDFFun out fn (DFEnvVar (IType TypeUnit) UnitLit:|[]) )-> do
        out' <- pureOut fn out
        return $ Fusion.Fun $ Ops.PureFusable [] (Ops.Call fn) out'
     -- Question: UnitLits can sneak in, in different forms but we never want them to appear in the output
     -- However I can't filterm them out here, because that breaks something in the downstream fusion and this something seems to be resposible to
     -- filter out unit literals in MOST cases. What's that something and shouldn't we have a common elimiation for ohua-introduces unit lits?
    {-(PureDFFun out fn (DFVar ( DataBinding (TBind _bnd TypeUnit)) :|[]) ) -> do
        out' <- pureOut fn out
        return $ Fusion.Fun $ Ops.PureFusable [] (Ops.Call fn) out'-}
    
    (PureDFFun out fn inp) -> do
        let args = toList $ map generateReceive inp
        out' <- pureOut fn out
        return $ Fusion.Fun $ Ops.PureFusable args (Ops.Call fn) out'
    (StateDFFun out fn (DFVar atBnd) inp) -> do
        let args = toList $ map generateReceive inp
            (TBind stateBnd stateTy) = unwrapTB atBnd
        (sOut, dataOut) <- stateOut fn out
        return $
          Fusion.Fun $
          Ops.STFusable
           (SRecv stateTy $ SChan stateBnd)
           args
           fn
           dataOut
           (SChan <$> sOut)
    otherFun -> throwError $ "Called conversion function with unsupported function "<> show otherFun
    where
      stateOut fn (sOut, dout) = do
        sOut' <- toDirect fn sOut
        dout' <- case dout of
                   Just dout' -> toList <$> pureOut fn dout'
                   Nothing -> return []
        return (sOut', dout')

      toDirect _ (Just (Direct bnd)) = return $ Just $ unwrapABnd bnd
      toDirect _ Nothing = return Nothing
      toDirect fn e = throwError $ "Unsupported multiple outputs for state on stateful function " <> show fn <> ": " <> show e

pureOut :: (ErrAndLogM m, Show a) => a -> OutData bty ty -> LoweringM m (NonEmpty (Ops.Result ty))
pureOut _ (Direct out) = return ((Ops.SendResult $ SChan (unwrapABnd out)) :| [])
pureOut _ (Destruct outs) = do 
  send_results <- mapM directToSendResult outs
  return send_results
pureOut _ (Dispatch outs) = return $ (Ops.DispatchResult $ map (SChan . unwrapABnd) outs) :| []
-- TODO support nested dispatch output for destruct (and limit it in DFLang. see issue ohua-core#28)
--pureOut fn e = throwError $ "Unsupported output configuration on function " <> show fn <> ": " <> show e

-- Basically the same as the above, but will error on non-directs for now, to not support
-- nested outputs 
-- ToDo: Check if restriction is needed
directToSendResult :: (ErrAndLogM m ) => OutData bty ty -> m ( Ops.Result ty)
directToSendResult  (Direct out) = return (Ops.SendResult $ SChan (unwrapABnd out))
directToSendResult  e = throwError $ "Unsupported output configuration on: " <> show e


generateReceive :: DFVar bty ty -> Ops.CallArg ty
generateReceive (DFVar atBnd) = 
  let (TBind argBnd argTy) = unwrapTB atBnd
  in Ops.Arg $ SRecv argTy $ SChan argBnd
-- Question: (Regarding FIXME) Wha and what can we do about it?
generateReceive (DFEnvVar _t l) = Ops.Converted $ Lit l -- FIXME looses type info!

generateArcsCode :: NormalizedDFExpr ty -> NonEmpty (Channel ty)
generateArcsCode = go
    where
        go (DFLang.Let app cont) =
            let collected = go cont
                collected' = HS.fromList $ NE.toList collected
                current = filter (not . (`HS.member` collected'))
                          $ manuallyDedup $ map (\tBnd -> asRecv (DataBinding tBnd)) $ insAndTypesDFApp app
            in foldl (flip (NE.<|)) collected current
        go (DFLang.Var atBnd) = 
            let (TBind bnd ty) = unwrapTB atBnd
            in SRecv ty (SChan bnd) :|[] -- result channel

        manuallyDedup :: [Com 'Recv ty] -> [Com 'Recv ty]
        manuallyDedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
-- FIXME see sertel/ohua-core#7: all these errors would immediately go away
generateNodeCode :: ErrAndLogM m => DFApp bty ty ->  LoweringM m (FusableExpr ty)
generateNodeCode e@(SMapFun (dOut,ctrlOut,sizeOut) inp) = do
    let input =
          case inp of
            (DFVar atBnd) -> 
              let (TBind bnd ty) = unwrapTB atBnd
              in Ops.Receive $ SRecv ty $ SChan bnd
            (DFEnvVar _t l) -> Ops.Expr $ Lit l
    dOut'    <- intoChan dOut
    dOut''   <- sequence (serializeDataOut <$> dOut')
    ctrlOut' <- maybe [] toList <$> intoChan ctrlOut
    sizeOut' <- maybe [] toList <$> intoChan sizeOut
    return $ SMap $ Ops.smapFun input dOut'' ctrlOut' sizeOut'
    where
      intoChan :: ErrAndLogM m => Maybe (OutData bty ty ) -> m (Maybe (NonEmpty (Com 'Channel ty)))
      intoChan o = do
        o' <- sequence (serializeOut <$> o)
        let o'' = map (SChan. asBnd) <$> o'
        return o''

      serializeDataOut :: ErrAndLogM m => NonEmpty (Com 'Channel ty) -> m (Com 'Channel ty)
      serializeDataOut (a :| []) = pure a
      serializeDataOut _ = throwError "We currently do not support destructuring and dispatch for loop data."

      serializeOut :: ErrAndLogM m => OutData b ty -> m (NonEmpty (TypedBinding ty))
      serializeOut Destruct{} = throwError $ "We currently do not support destructuring on loop data: " <> show e
      serializeOut o = pure $ toOutBnds o

generateNodeCode e@(PureDFFun out (FunRef funName  _fType) inp) | funName == collect = do
    (sizeIn, dataIn) <-
        case inp of
            (DFVar stateTBind :| [DFVar dataTBind]) ->
              let (TBind sbnd sty) = unwrapTB stateTBind
                  (TBind dbnd dty) = unwrapTB dataTBind
              in return (SRecv sty $ SChan sbnd, SRecv dty $ SChan dbnd)
            _ -> invariantBroken $ "Collect arguments don't match:\n" <> show e
    collectedOutput <-
        case out of
            Direct x -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "Collect outputs don't match:\n" <> show e
    return $ SMap $ Ops.collect dataIn sizeIn collectedOutput

generateNodeCode e@(IfFun out inp) = do
    condIn <-
        case inp of
            (DFVar atBnd) -> return $ asRecv atBnd
            _ -> invariantBroken $ "envars as conditional input not yet supported:\n" <> show e
    outs <-
        case out of
            (Direct t, Direct fa) -> return $ (SChan $ unwrapABnd t, SChan $ unwrapABnd fa) :| []
            (Dispatch tr, Dispatch fa) ->
              if NE.length tr == NE.length fa
              then return $ NE.map (\(t,_f) -> (SChan $ unwrapABnd t, SChan $ unwrapABnd t)) $ NE.zip tr fa
              else invariantBroken $ "unbalanced control signal dispatch:\n " <> (toStrict $ prettyExpr e)
            _ -> invariantBroken $ "conditional controls are never destructed but got:\n" <> (toStrict $ prettyExpr e)
    return $ Unfusable $
        EndlessLoop $
            Ops.ifFun condIn outs

generateNodeCode e@(PureDFFun out (FunRef funName  _fType) inp) | funName == select = do
    (condIn, trueIn, falseIn) <-
        case inp of
            (DFVar xATBnd :| [DFVar yATBnd, DFVar zATBnd]) ->
                return
                    ( asRecv xATBnd
                    , asRecv yATBnd
                    , asRecv zATBnd)
            -- QUESTION: Why does it say 'don't match'. This function isn't typechecking
            -- whether x is a bool and y and z have the same type.
            _ -> invariantBroken $ "Select arguments don't match:\n" <> show e
    out' <-
        case out of
            (Direct bnd) -> return $ SChan $ unwrapABnd bnd
            -- QUESTION: Again 'match' what?
            _ -> invariantBroken $ "Select outputs don't match:\n" <> show e
    return $ Unfusable $
        EndlessLoop $
            Ops.select condIn trueIn falseIn out'

generateNodeCode e@(PureDFFun out (FunRef fun _) inp) | fun == IFuns.seqFun = do
  out' <- case out of
           Direct x -> return $ SChan $ unwrapABnd x
           _ -> invariantBroken $ "Seq must only have one output:\n" <> show e
  case inp of
    DFVar stmtInATBind :| [DFEnvVar _ty lit] ->
      return $
        Unfusable $
        Stmt (ReceiveData $ asRecv stmtInATBind) $
        BLang.Let "x" (Lit lit) $
        SendData $ SSend out' $ Left "x"
    _ -> invariantBroken $
            "Seq must have two inputs where the second is a literal:\n" <> show e

generateNodeCode e@(PureDFFun out (FunRef funName _fType) inp) | funName == IFuns.runSTCLangSMap = do
--    (sizeIn, stateIn) <-
    out' <-
        case out of
            Direct x -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "STCLangSMap outputs don't match:\n" <> show e
    case inp of
      (DFVar xATBnd :| [DFVar yATBnd ]) ->
        return $ Unfusable $ Ops.genSTCLangSMap $
        Ops.STCLangSMap
        (asRecv xATBnd)
        (asRecv yATBnd)
        out'
      (DFVar xATBnd :| []) ->
        return $ STC $ Ops.FusableSTCLangSMap (asRecv xATBnd) out'
      _ -> invariantBroken $ "STCLangSMap arguments don't match:\n" <> show e

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

generateNodeCode e@(PureDFFun out (FunRef funName _fType) inp) | funName == ctrl = do
    out' <-
        case out of
            Direct x -> return $ SChan $ unwrapABnd x
            Destruct (Direct x :| []) -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "Control outputs don't match:\n" <> show e
    case inp of
        DFVar ctrlATBnd :| [DFVar inpATBnd] ->
            return $ Control $ Left $
                Ops.mkCtrl
                    (asRecv ctrlATBnd)
                    (asRecv inpATBnd)
                    out'
        DFVar ctrlATBnd :| [DFEnvVar _ti lit] ->
            return $ Control $ Right $
                Ops.mkLittedCtrl (asRecv ctrlATBnd) lit out' -- FIXME loosing the semantic type here!
        _ -> invariantBroken $ "Control arguments don't match:\n" <> show e

generateNodeCode e@(PureDFFun out (FunRef funName  _fType) inp) | funName == IFuns.seqFun = do
  out' <- case out of
           Direct x -> return $ SChan $ unwrapABnd x
           _ -> invariantBroken $ "Seq must only have one output:\n" <> show e
  case inp of
    DFVar inpATBnd :| [DFEnvVar _ l] ->
      return $
        Unfusable $
        Stmt (ReceiveData $ asRecv inpATBnd) $
        BLang.Let "x" (Lit l) $
        SendData $ SSend out' $ Left "x"
    _ -> invariantBroken $
            "Seq must have two inputs where the second is a literal:\n" <> show e

generateNodeCode e@(PureDFFun out (FunRef funName  _fType) inp) | funName == IFuns.unitFun = do
  case inp of
   DFEnvVar _t (FunRefLit f@(FunRef p _)) :| [v] | p == IFuns.id ->
     generateNodeCode $ PureDFFun out f (v:|[])

   (DFEnvVar _t (FunRefLit pr@(FunRef _n  _ty)) :| [v]) -> -- FIXME this feels like a bug to me. why do we take this detour via unitFun???
     -- For some reason the added UnitLit argument might arive here as DFVar or DFEnvVar 
     -- so we can either repace both by one of theme here to catch only one case in 'generateFunctionCode', or catch both patterns later 
     -- I prefer the second option, since I Don't know if there's any other way a UnitLit can sneak in as an argument and it is always wrong to
      -- kepp them
     generateFunctionCode $ PureDFFun out pr (v:|[])
     
   _ -> invariantBroken $ "unknown function as first argument or wrong number of arguments (expected 2) to unitFun:\n" <> show e

generateNodeCode (PureDFFun out (FunRef funName _fType) (inp:|[])) | funName == IFuns.id = do
  out' <- pureOut funName out
  case inp of
    DFEnvVar _t l ->
      return $
      Fusion.Fun $
      Ops.IdFusable (Ops.Converted $ Lit l) out'
    DFVar atbnd ->
      return $
      Fusion.Fun $
      Ops.IdFusable (Ops.Arg $ asRecv atbnd) out'

generateNodeCode (PureDFFun out fn@(FunRef funName  funTy) inp) | funName == IFuns.tupleFun = do
  let args = toList $ map generateReceive inp
  out' <- pureOut fn out
  return $ Fusion.Fun $ Ops.PureFusable args (Ops.Tup funTy) out'

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
    ctrlOut' <- sequence (directOut <$> ctrlOut)
    recArgsOuts' <- mapM directOut $ V.toList recArgsOuts
    let recInitArgsIns' = map varToChanOrLit $ V.toList recInitArgsIns
    -- TODO we could allow literals in the recursion args as well but we should require one to be a variable that got computed!
    recArgsIns' <- mapM varToChan $ V.toList recArgsIns
    recCondIn' <- varToChan recCondIn -- not allowing literals here to prevent at some form of infinite loop
    -- note that this aspect is interesting:
    -- normally, I could just say that the result of a recursion may just be a literal.
    -- but then what would that recursion actually compute?!

    -- Answer: An example of this our server loop -> it recurses for the side effects only and returns a unit 

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
        directOut :: ErrAndLogM m => OutData bty ty -> LoweringM m (Com 'Channel ty)
        directOut x = case x of
                        Direct x' -> return $ SChan $ unwrapABnd x'
                        _ -> invariantBroken $ "Control outputs don't match:\n" <> show e
        -- directOut' Nothing = pure Nothing
        varToChanOrLit :: DFVar bty ty -> Either (Com 'Recv ty) (Lit ty 'Resolved)
        varToChanOrLit (DFVar atbnd) = Left $ asRecv atbnd
        varToChanOrLit (DFEnvVar _ l) = Right l

        varToChan (DFVar atbnd) = return $ asRecv atbnd
        -- FIXME the below case needs to be checked during checking the well-formedness of the recursion and then carried along properly in the type.
        varToChan v = invariantBroken $ "environment variable not allowed in this position for recursion: " <> show v



generateNodeCode e = generateFunctionCode e

asRecv :: ATypedBinding bty ty -> Com 'Recv ty
asRecv  atBnd = 
  let (TBind bnd ty) = unwrapTB atBnd
  in SRecv ty $ SChan bnd 

asSend :: ATypedBinding bty ty -> Com 'Send ty
asSend  atBnd = 
  let (TBind bnd _ty) = unwrapTB atBnd
  in SSend (SChan bnd) (Left bnd) 