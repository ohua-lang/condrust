{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module Ohua.Backend.Operators.Function where

import Ohua.Prelude hiding (First, Second)

import Ohua.Backend.Lang as L

import qualified Data.List.NonEmpty as NE


-- FIXME Seems to be like this: Arg Recv (Either Recv TaskExpr) | Drop (Either Recv TaskExpr)
data CallArg ty
    = Arg (Com 'Recv ty) | Drop (Either (Com 'Recv ty) (TaskExpr ty)) | Converted (TaskExpr ty)
    deriving (Eq, Generic)

instance Hashable (CallArg ty)

data Result ty
  = SendResult (Com 'Channel ty)
  | DispatchResult (NonEmpty (Com 'Channel ty))
  -- | This happens during fusion. But we still need to preserve the index positions otherwise
  --   the destructuring breaks.
  | DropResult
  deriving (Eq, Generic, Show)

instance Hashable (Result ty)

data FunCall ty = Call (FunRef ty) | Tup (FunType ty) deriving (Eq, Generic, Show, Hashable)

-- TODO:
--   1) no functions in data type -> done
--   2) deriving instance ... -> done
--   3) take over OutData
--   4) implement dispatch and destructuring via nth and ...?
--   5) certainly this definition of a function really is no different
--      from the newly defined DFLang! (one more reason to move this into common.)

data FusFunction sin ty
    = PureFusable
        [CallArg ty]  -- data receive
        (FunCall ty)
        (NonEmpty (Result ty)) -- send result
    | STFusable
        (sin ty) -- state receive
        [CallArg ty]  -- data receive
        (FunRef ty)
        [Result ty] -- send result
        (Maybe (Com 'Channel ty)) -- send state
    | IdFusable
        (CallArg ty)
        (NonEmpty (Result ty))
    deriving (Generic)

-- using a vector would have been so much nicer, but implementing Eq and Hashable
-- manually is just a pain.
type FusableFunction ty = FusFunction (Com 'Recv) ty
type FusedFunction ty = FusFunction CallArg ty

deriving instance Hashable (FusableFunction ty)
deriving instance Eq (FusableFunction ty)

channels :: Result ty -> [Com 'Channel ty]
channels (SendResult c) = [c]
channels (DispatchResult cs) = toList cs
channels DropResult = []

toFuseFun :: FusableFunction ty -> FusedFunction ty
toFuseFun (PureFusable recvs qb outs) = PureFusable recvs qb outs
toFuseFun (STFusable a b c d e) = STFusable (Arg a) b c d e
toFuseFun (IdFusable recv outs) = IdFusable recv outs

genFun :: FusableFunction ty -> TaskExpr ty
genFun fun = loop (funReceives fun) $ (\f -> genFun' (genSend f) f) $ toFuseFun fun

funReceives :: FusableFunction ty -> [Com 'Recv ty]
funReceives (PureFusable vars _ _)   = extractAll vars
funReceives (STFusable r vars _ _ _) = r : extractAll vars
funReceives (IdFusable r _) = extractAll [r]

loop :: [Com 'Recv ty] -> TaskExpr ty -> TaskExpr ty
loop [] c = c
loop _  c = EndlessLoop c

result = "res"

genSend :: forall ty.FusedFunction ty -> TaskExpr ty
genSend = \case
    (PureFusable _ _ o) -> dataOut (toList o) $ Lit UnitLit
    (STFusable stateRecv receives _ sendRes sendState) ->
        let varsAndReceives =
              NE.zipWith (curry generateReceiveCode) (0 :| [1 ..]) $ stateRecv :| receives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
        in dataOut sendRes $
           maybe (Lit UnitLit) (SendData . (`SSend` Left stateArg)) sendState
    (IdFusable _ o) -> dataOut (toList o) $ Lit UnitLit
    where
      dataOut :: [Result ty] -> TaskExpr ty -> TaskExpr ty
      dataOut [] ct = ct
      dataOut [DropResult] ct = ct
      dataOut [SendResult (SChan b)] ct = Stmt (SendData $ SSend (SChan b) (Left b)) ct
      dataOut [DispatchResult chans] ct =
        foldr
        (\chan cont -> Stmt (SendData $ SSend chan (Left result)) cont)
        ct
        chans
      dataOut [out1, out2] ct =
        getOut out1 firstIndexing $
        getOut out2 secondIndexing
        ct
      dataOut outs ct = foldr
        (\(out, num) expr -> getOut out (`Indexing` num) expr) ct (zip outs [0 ..])

      getOut :: Result ty -> (Binding -> TaskExpr ty) -> TaskExpr ty -> TaskExpr ty
      getOut DropResult _ ct = ct
      getOut (DispatchResult chans) f ct = foldr (\chan cont -> getOut (SendResult chan) f cont) ct chans
      getOut (SendResult (SChan b)) f ct =
        Let b (f result) $
        Stmt (SendData $ SSend (SChan b) (Left b)) ct

genFun'' :: FusableFunction ty -> TaskExpr ty
genFun'' fun = (\f -> genFun' (genSend f) f) $ toFuseFun fun

genFunWithCont :: TaskExpr ty -> FusableFunction ty -> TaskExpr ty
genFunWithCont ct fun = loop (funReceives fun) $ genFun' ct $ toFuseFun fun

genFun' :: TaskExpr ty -> FusedFunction ty -> TaskExpr ty
genFun' ct = \case
    (PureFusable receives f out) ->
        let varsAndReceives = zipWith (curry generateReceiveCode) [0 ..] receives
            call = case f of
                     (Call (FunRef app _ ty)) ->
                       Apply $ Stateless app $ getCallArgs Var ty varsAndReceives
                     (Tup ty) ->
                       case getCallArgs id ty varsAndReceives of
                         [] -> Lit UnitLit
                         (a:as) -> Tuple  $ flip NE.map (a:|as) $ \case
                           b -> Left b
        in flip letReceives varsAndReceives $
           callWithResult (toList out) call ct
    (STFusable stateRecv receives (FunRef app _ funTy) sendRes _sendState) ->
        let varsAndReceives =
              NE.zipWith (curry generateReceiveCode) (0 :| [1 ..]) $ stateRecv :| receives
            callArgs = getCallArgs Var funTy $ NE.tail varsAndReceives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
            call = (Apply $ Stateful (Var stateArg) app callArgs)
        in flip letReceives (toList varsAndReceives) $
           callWithResult sendRes call ct
    (IdFusable i o) ->
        let varAndReceive@(_,v,_) = generateReceiveCode (0,i)
        in flip letReceives [varAndReceive]
           $ callWithResult (NE.toList o) (Var v) ct
    where
        getCallArgs p (FunType (Left Unit)) _ = []
        getCallArgs p (STFunType _ (Left Unit)) _ = []
        getCallArgs p _ vrs =
          map (\(_,v,_) -> p v) $
          filter (\case (Drop _, _, _) -> False; _ -> True)
          vrs
        letReceives = foldr ((\ (v, r) c -> Let v r c) . (\(_,v,r) -> (v,r)))
        callWithResult out call ct =
          case filter (/= DropResult) out of
            [] -> Stmt call ct
            _ -> case out of
                   [SendResult (SChan b)] -> Let b call ct
                   cs -> Let result call ct

generateReceiveCode :: (Show a) => (a, CallArg ty) -> (CallArg ty, Binding, TaskExpr ty)
generateReceiveCode (idx, a@(Arg r)) = (a, "var_" <> show idx, ReceiveData r)
generateReceiveCode (idx, a@(Drop (Left r))) = (a, "_var_" <> show idx, ReceiveData r)
generateReceiveCode (idx, a@(Drop (Right e))) = (a, "_var_" <> show idx, e)
generateReceiveCode (idx, a@(Converted e)) = (a, "var_" <> show idx, e)

extractAll :: [CallArg ty] -> [Com 'Recv ty]
extractAll = mapMaybe extractOne

extractOne :: CallArg ty -> Maybe (Com 'Recv ty)
extractOne (Arg r) = Just r
extractOne (Drop (Left r)) = Just r
extractOne (Drop _) = Nothing
extractOne (Converted _) = Nothing

data FusedFun ty
    = FusedFun (FusedFunction ty) (TaskExpr ty)

genFused :: FusedFunction ty -> TaskExpr ty
genFused fun = genFun' (genSend fun) fun

genFusedFun' :: FusedFun ty -> TaskExpr ty
genFusedFun' (FusedFun fun ct) = genFun' ct fun

genFusedFun :: FusedFun ty -> TaskExpr ty
genFusedFun f@(FusedFun fun ct) = loop (fusedFunReceives fun) $ genFusedFun' f

fusedFunReceives :: FusedFunction ty -> [Com 'Recv ty]
fusedFunReceives (PureFusable vars _ _)   = extractAll vars
fusedFunReceives (STFusable r vars _ _ _) = extractAll $ r:vars
fusedFunReceives (IdFusable r _) = extractAll [r]

fuseFuns :: FusableFunction ty -> FusableFunction ty -> FusedFun ty
fuseFuns fun1 fun2 =
  case (fun1, fun2) of
    -- | Fusion for state initializers
    ( PureFusable initReceives fInit usOuts@(SendResult usOut@(SChan stateCh) :| []),
      STFusable stateRecv@(SRecv _ dsStateIn) argReceives fState outs stateOut ) |
      usOut == dsStateIn
      ->
      FusedFun
      (PureFusable initReceives fInit (DropResult :| [] ))
      (genFused $ STFusable (Converted $ Var stateCh) argReceives fState outs stateOut)
    _ -> error "Unimplemented function called. Please report"
