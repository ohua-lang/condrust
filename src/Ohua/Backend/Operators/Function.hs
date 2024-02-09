{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Ohua.Backend.Operators.Function where

import Ohua.Commons.Prelude hiding (First, Second)

import Ohua.Backend.Lang as L

import qualified Data.List.NonEmpty as NE


-- FIXME Seems to be like this: Arg Recv (Either Recv TaskExpr) | Drop (Either Recv TaskExpr)
data CallArg embExpr ty
    = Arg (Com 'Recv embExpr ty) | Drop (Either (Com 'Recv embExpr ty) (TaskExpr embExpr ty)) | Converted (TaskExpr embExpr ty)
    deriving (Eq, Generic, Show)

instance Hashable (CallArg embExpr ty)

data Result embExpr ty
  = SendResult (Com 'Channel embExpr ty)
  | DispatchResult (NonEmpty (Com 'Channel embExpr ty))
  -- | This happens during fusion. But we still need to preserve the index positions otherwise
  --   the destructuring breaks.
  | DropResult
  deriving (Eq, Generic, Show)

instance Hashable (Result embExpr ty)

data FunCall ty = Call (FunRef ty Resolved) | Tup (FunType ty Resolved) deriving (Eq, Generic, Show, Hashable)

-- TODO:
--   1) no functions in data type -> done
--   2) deriving instance ... -> done
--   3) take over OutData
--   4) implement dispatch and destructuring via nth and ...?
--   5) certainly this definition of a function really is no different
--      from the newly defined DFLang! (one more reason to move this into common.)

data FusFunction sin ty embExpr
    = PureFusable
        [CallArg embExpr ty]  -- data receive
        (FunCall ty)
        (NonEmpty (Result embExpr ty)) -- send result
    | STFusable
        (sin ty) -- state receive
        [CallArg embExpr ty]  -- data receive
        (FunRef ty Resolved)
        [Result embExpr ty] -- send result
        (Maybe (Com 'Channel embExpr ty)) -- send state
    | IdFusable
        (CallArg embExpr ty)
        (NonEmpty (Result embExpr ty))
    deriving (Generic)

deriving instance (Show (sin ty), Show ty, Show  embExpr) => Show (FusFunction sin ty embExpr) 

-- using a vector would have been so much nicer, but implementing Eq and Hashable
-- manually is just a pain.
type FusableFunction embExpr ty = FusFunction (Com 'Recv embExpr) ty embExpr
type FusedFunction embExpr ty = FusFunction (CallArg embExpr) ty embExpr

deriving instance Hashable (FusableFunction embExpr ty)
deriving instance Eq (FusableFunction embExpr ty)

channels :: Result embExpr ty -> [Com 'Channel embExpr ty]
channels (SendResult c) = [c]
channels (DispatchResult cs) = toList cs
channels DropResult = []

toFuseFun :: FusableFunction embExpr ty -> FusedFunction embExpr ty
toFuseFun (PureFusable recvs qb outs) = PureFusable recvs qb outs
toFuseFun (STFusable a b c d e) = STFusable (Arg a) b c d e
toFuseFun (IdFusable recv outs) = IdFusable recv outs

genFun :: (Show ty, Show embExpr) => FusableFunction embExpr ty -> TaskExpr embExpr ty
genFun fun = loop (funReceives fun) $ (\f -> genFun' (genSend f) f) $ toFuseFun fun

funReceives :: FusableFunction embExpr ty -> [Com 'Recv embExpr ty]
funReceives (PureFusable vars _ _)   = extractAll vars
funReceives (STFusable r vars _ _ _) = r : extractAll vars
funReceives (IdFusable r _) = extractAll [r]

loop :: [Com 'Recv embExpr ty] -> TaskExpr embExpr ty -> TaskExpr embExpr ty
loop [] c = c
loop _  c = EndlessLoop c

result = "res"

genSend :: forall embExpr ty. (Show embExpr) =>  FusedFunction embExpr ty -> TaskExpr embExpr ty
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
      dataOut :: [Result embExpr ty] -> TaskExpr embExpr ty -> TaskExpr embExpr ty
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

      getOut :: Result embExpr ty -> (Binding -> TaskExpr embExpr ty) -> TaskExpr embExpr ty -> TaskExpr embExpr ty
      getOut DropResult _ ct = ct
      getOut (DispatchResult chans) f ct = foldr (\chan cont -> getOut (SendResult chan) f cont) ct chans
      getOut (SendResult (SChan b)) f ct =
        Let b (f result) $
        Stmt (SendData $ SSend (SChan b) (Left b)) ct

genFun'' ::(Show embExpr) => FusableFunction embExpr ty -> TaskExpr embExpr ty
genFun'' fun = (\f -> genFun' (genSend f) f) $ toFuseFun fun

genFunWithCont ::(Show embExpr) => TaskExpr embExpr ty -> FusableFunction embExpr ty -> TaskExpr embExpr ty
genFunWithCont ct fun = loop (funReceives fun) $ genFun' ct $ toFuseFun fun

genFun' :: (Show embExpr) => TaskExpr embExpr ty -> FusedFunction embExpr ty -> TaskExpr embExpr ty
genFun' ct = \case
    (PureFusable receives f out) ->
        let varsAndReceives = zipWith (curry generateReceiveCode) [0 ..] receives
            call = case f of
                     (Call (FunRef app ty)) ->
                       Apply $ Stateless app $ getCallArgs Var ty varsAndReceives
                     (Tup ty) ->
                       case getCallArgs id ty varsAndReceives of
                         [] -> Lit UnitLit
                         (a:as) -> Tuple  $ flip NE.map (a:|as) $ \case
                           b -> Left b
        in flip letReceives varsAndReceives $
           callWithResult (toList out) call ct
    (STFusable stateRecv receives (FunRef app funTy) sendRes _sendState) ->
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
        getCallArgs p (FunType (Left ()) _ ) _ = []
        getCallArgs p (STFunType _ (Left ()) _retTy) _ = []
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

generateReceiveCode :: (Show a, Show embExpr) => (a, CallArg embExpr ty) -> (CallArg embExpr ty, Binding, TaskExpr embExpr ty)
generateReceiveCode (idx, a@(Arg r)) = (a, "var_" <> show idx, ReceiveData r)
generateReceiveCode (idx, a@(Drop (Left r))) = (a, "_var_" <> show idx, ReceiveData r)
generateReceiveCode (idx, a@(Drop (Right e))) = (a, "_var_" <> show idx, e)
generateReceiveCode (idx, a@(Converted e)) = (a, "var_" <> show idx, e)

extractAll :: [CallArg embExpr ty] -> [Com 'Recv embExpr ty]
extractAll = mapMaybe extractOne

extractOne :: CallArg embExpr ty -> Maybe (Com 'Recv embExpr ty)
extractOne (Arg r) = Just r
extractOne (Drop (Left r)) = Just r
extractOne (Drop _) = Nothing
extractOne (Converted _) = Nothing

data FusedFun embExpr ty
    = FusedFun (FusedFunction embExpr ty) (TaskExpr embExpr ty) deriving Show

genFused :: (Show embExpr) => FusedFunction embExpr ty -> TaskExpr embExpr ty
genFused fun = genFun' (genSend fun) fun

genFusedFun' :: (Show embExpr) => FusedFun embExpr ty -> TaskExpr embExpr ty
genFusedFun' (FusedFun fun ct) = genFun' ct fun

genFusedFun :: (Show ty, Show embExpr) => FusedFun embExpr ty -> TaskExpr embExpr ty
genFusedFun f@(FusedFun fun ct) = trace ("Processing task in genFusedFun: \n" <> show fun) loop (fusedFunReceives fun) $ genFusedFun' f

fusedFunReceives :: FusedFunction embExpr ty -> [Com 'Recv embExpr ty]
fusedFunReceives (PureFusable vars _ _)   = extractAll vars
fusedFunReceives (STFusable r vars _ _ _) = extractAll $ r:vars
fusedFunReceives (IdFusable r _) = extractAll [r]

fuseFuns :: (Show embExpr) => FusableFunction embExpr ty -> FusableFunction embExpr ty -> Maybe (FusedFun embExpr ty)
fuseFuns fun1 fun2 =
  case (fun1, fun2) of
    -- | Fusion for state initializers
    ( PureFusable initReceives fInit usOuts@(SendResult usOut@(SChan stateCh) :| []),
      STFusable stateRecv@(SRecv _ dsStateIn) argReceives fState outs stateOut ) |
      usOut == dsStateIn
      ->
      Just $
      FusedFun
      (PureFusable initReceives fInit (DropResult :| [] ))
      (genFused $ STFusable (Converted $ Var stateCh) argReceives fState outs stateOut)
    _ -> Nothing
