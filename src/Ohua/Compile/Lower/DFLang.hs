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

generateFunctionCode :: forall ty a m. CompM m => DFApp a ty -> LoweringM m (FusableExpr ty)
generateFunctionCode = \case
    (PureDFFun out fn inp) -> do
        let args = toList $ map generateReceive inp
        out' <- pureOut fn out
        return $ Fusion.Fun $ Ops.PureFusable args fn out'
    (StateDFFun out fn (DFVar stateT stateIn) inp) -> do
        let args = toList $ map generateReceive inp
        (sOut, dataOut) <- stateOut fn out
        return $
          Fusion.Fun $
          Ops.STFusable
           (SRecv stateT $ SChan $ unwrapABnd stateIn)
           args
           fn
           dataOut
           (SChan <$> sOut)
    where
      pureOut _ (Direct out) = return (SChan (unwrapABnd out) :| [])
      pureOut _ (Destruct [Direct out1, Direct out2]) =
        return (SChan (unwrapABnd out1) :| [SChan $ unwrapABnd out1])
      pureOut fn e = throwError $ "Unsupported (more than 2) data outputs on function " <> show fn <> ": " <> show e

      stateOut fn (sOut, dout) = do
        sOut' <- toDirect fn sOut
        dout' <- case dout of
                   Just dout' -> toList <$> pureOut fn dout'
                   Nothing -> return []
        return (sOut', dout')

      toDirect _ (Just (Direct bnd)) = return $ Just $ unwrapABnd bnd
      toDirect _ Nothing = return Nothing
      toDirect fn e = throwError $ "Unsupported multiple outputs for state on stateful function " <> show fn <> ": " <> show e

generateReceive :: DFVar semTy ty -> Ops.CallArg ty
generateReceive (DFVar t bnd) =
    Ops.Arg $ SRecv t $ SChan $ unwrapABnd bnd
generateReceive (DFEnvVar t l) = Ops.Converted $ Lit l -- FIXME looses type info!

generateArcsCode :: NormalizedDFExpr ty -> NonEmpty (Channel ty)
generateArcsCode = traceShowId . go
    where
        go (DFLang.Let app cont) =
            let collected = go cont
                collected' = HS.fromList $ NE.toList collected
                current = filter (not . (`HS.member` collected'))
                          $ manuallyDedup $ map (\(t,b) -> SRecv t $ SChan b) $ insAndTypesDFApp app
            in foldl (flip (NE.<|)) collected current
        go (DFLang.Var bnd) = SRecv TypeVar (SChan bnd) :|[] -- result channel

        manuallyDedup :: [Com 'Recv ty] -> [Com 'Recv ty]
        manuallyDedup xs = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] xs
-- FIXME see sertel/ohua-core#7: all these errors would immediately go away
generateNodeCode :: CompM m => DFApp semTy ty ->  LoweringM m (FusableExpr ty)
generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp)  | fun == smapFun = do
    input <-
        case inp of
            [x] -> case x of
                        (DFVar t v) -> return $ SRecv t $ SChan $ unwrapABnd v
                        _ -> invariantBroken $ "Input to SMap must var not literal:\n" <> show e
            _ -> invariantBroken $ "SMap should only have a single input." <> show e
    (dataOut, ctrlOut, collectOut) <-
        case out of -- FIXME: I can not even be sure here whether the order is the correct one!
            Destruct [Direct x, Direct y, Direct z] ->
                return ( Just $ SChan $ unwrapABnd x
                       , [SChan $ unwrapABnd y]
                       , [SChan $ unwrapABnd z]
                       )
            Destruct [Direct x, Dispatch ct, Dispatch size] ->
              return ( Just $ wrap x
                     , map wrap $ NE.toList ct
                     , map wrap $ NE.toList size
                     )
            Destruct [Direct x, Dispatch ct, Direct size] ->
              return ( Just $ wrap x
                     , map wrap $ NE.toList ct
                     , [wrap size]
                     )
            Destruct [Direct x, Direct ct, Dispatch size] ->
              return ( Just $ wrap x
                     , [wrap ct]
                     , map wrap $ NE.toList size
                     )
            -- see hack in dead code elimination
            Destruct [Direct ct, Direct size] ->
              return ( Nothing
                     , [SChan $ unwrapABnd ct]
                     , [SChan $ unwrapABnd size]
                     )
            Destruct [Dispatch ct, Dispatch size] ->
              return ( Nothing
                     , map wrap $ NE.toList ct
                     , map wrap $ NE.toList size
                     )
            Destruct [Dispatch ct, Direct size] ->
              return ( Nothing
                     , map wrap $ NE.toList ct
                     , [wrap size]
                     )
            Destruct [Direct ct, Dispatch size] ->
              return ( Nothing
                     , [wrap ct]
                     , map wrap $ NE.toList size
                     )
            Destruct [Direct ds] ->
              return ( Just $ SChan $ unwrapABnd ds
                     , []
                     , []
                     )
            _ -> invariantBroken $ "SMap must have 3 outputs:\n" <> show e
    return $ SMap $ Ops.smapFun input dataOut ctrlOut collectOut
    where
      wrap b = SChan $ unwrapABnd b

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == collect = do
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

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == ifFun = do
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

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == select = do
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

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == Refs.runSTCLangSMap = do
--    (sizeIn, stateIn) <-
    out' <-
        case out of
            Direct x -> return $ SChan $ unwrapABnd x
            _ -> invariantBroken $ "STCLangSMap outputs don't match:\n" <> show e
    case inp of
      [DFVar xType x, DFVar yType y] ->
        return $ Unfusable $ Ops.genSTCLangSMap $
        Ops.STCLangSMap
        (SRecv xType $ SChan $ unwrapABnd x)
        (SRecv yType $ SChan $ unwrapABnd y)
        out'
      [DFVar xType x] ->
        return $ STC $ Ops.FusableSTCLangSMap (SRecv xType $ SChan $ unwrapABnd x) out'
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

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == ctrl = do
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

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == Refs.seqFun = do
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

generateNodeCode e@(PureDFFun out (FunRef fun _ _) inp) | fun == Refs.unitFun = do
  out' <- case out of
           Direct x -> return $ SChan $ unwrapABnd x
           _ -> invariantBroken $ "unitFun must only have one output:\n" <> show e
  case inp of
   [DFEnvVar _t (FunRefLit (FunRef p _ _)), v] | p == Refs.id ->
     case v of
        (DFVar t bnd) ->
          return $
            Fusion.Fun $
            Ops.IdFusable (Ops.Arg $ SRecv t $ SChan $ unwrapABnd bnd) $ out' :| []
        (DFEnvVar _ l) ->
          return $
            Fusion.Fun $
            Ops.IdFusable (Ops.Converted $ Lit l) $ out' :| []
   [DFEnvVar _t (FunRefLit pr@(FunRef p _ _)), v] -> -- FIXME this feels like a bug to me. why do we take this detour via unitFun???
     generateFunctionCode $ PureDFFun out pr (v:|[])
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
    ctrlOut' <- sequence (directOut <$> ctrlOut)
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
        directOut' Nothing = pure Nothing
        varToChanOrLit :: DFVar a ty -> Either (Com 'Recv ty) (Lit ty)
        varToChanOrLit (DFVar t v) = Left $ SRecv t $ SChan $ unwrapABnd v
        varToChanOrLit (DFEnvVar _ l) = Right l
        varToChan (DFVar t v) = return $ SRecv t $ SChan $ unwrapABnd v
        -- FIXME the below case needs to be checked during checking the well-formedness of the recursion and then carried along properly in the type.
        varToChan v = invariantBroken $ "environment variable not allowed in this position for recursion: " <> show v
generateNodeCode e = generateFunctionCode e

