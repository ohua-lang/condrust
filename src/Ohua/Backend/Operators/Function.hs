{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ohua.Backend.Operators.Function where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)

import qualified Data.List.NonEmpty as NE


-- FIXME Seems to be like this: Arg Recv (Either Recv TaskExpr) | Drop (Either Recv TaskExpr)
data CallArg ty
    = Arg (Com 'Recv ty) | Drop (Either (Com 'Recv ty) (TaskExpr ty)) | Converted (TaskExpr ty)
    deriving (Eq, Generic)

instance Hashable (CallArg ty)

-- TODO:
--   1) no functions in data type -> done
--   2) deriving instance ... -> done
--   3) take over OutData
--   4) implement dispatch and destructuring via nth and ...?
--   5) certainly this definition of a function really is no different
--      from the newly defined DFLang! (one more reason to move this into common.)

data FusFunction f ty
    = PureFusable
        [CallArg ty]  -- data receive
        QualifiedBinding
        (f (Com 'Channel ty)) -- send result
    | STFusable
        (Com 'Recv ty) -- state receive
        [CallArg ty]  -- data receive
        QualifiedBinding
        (Maybe (Com 'Channel ty)) -- send result
        (Maybe (Com 'Channel ty)) -- send state
    deriving (Generic)

type FusableFunction ty = FusFunction Identity ty
type FusedFunction ty = FusFunction Maybe ty

deriving instance Hashable (FusableFunction ty)
deriving instance Eq (FusableFunction ty)

toFuseFun :: FusableFunction ty -> FusedFunction ty
toFuseFun (PureFusable recvs qb (Identity out)) = PureFusable recvs qb (Just out)
toFuseFun (STFusable a b c d e) = STFusable a b c d e

genFun :: FusableFunction ty -> TaskExpr ty
genFun fun = loop fun $ (\f -> genFun' (genSend f) f) $ toFuseFun fun

loop :: FusFunction f ty -> TaskExpr ty -> TaskExpr ty
loop fun c = if null $ funReceives fun
        then c
        else EndlessLoop c

genSend :: FusedFunction ty -> TaskExpr ty
genSend = \case
    (PureFusable _ _ o) -> maybe (Lit UnitLit) (\out@(SChan b) -> SendData $ SSend out b) o
    (STFusable stateRecv receives _ sendRes sendState) -> 
        let varsAndReceives = NE.zipWith (curry generateReceiveCode) [0 ..] $ Arg stateRecv :| receives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
        in foldr Stmt (Lit UnitLit) $ 
            catMaybes [SendData . (\o@(SChan b) -> o `SSend` b) <$> sendRes, SendData . (`SSend` stateArg) <$> sendState]

genFun'' :: FusableFunction ty -> TaskExpr ty
genFun'' fun = (\f -> genFun' (genSend f) f) $ toFuseFun fun

genFun' :: TaskExpr ty -> FusedFunction ty -> TaskExpr ty
genFun' ct = \case
    (PureFusable receives app out) ->
        let varsAndReceives = zipWith (curry generateReceiveCode) [0 ..] receives
            callArgs = getCallArgs varsAndReceives
            call = Apply $ Stateless app callArgs
        in flip letReceives varsAndReceives $
            maybe
                (Stmt call ct)
                (\(SChan b) -> Let b call ct)
                out
    (STFusable stateRecv receives app sendRes _sendState) ->
        let varsAndReceives = NE.zipWith (curry generateReceiveCode) [0 ..] $ Arg stateRecv :| receives
            callArgs = getCallArgs $ NE.tail varsAndReceives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
            call = (Apply $ Stateful (Var stateArg) app callArgs) 
        in flip letReceives (toList varsAndReceives) $
            maybe 
                (Stmt call ct) 
                (\(SChan b) -> Let b call ct)
                sendRes
    where
        getCallArgs = map (\(_,v,_) -> Var v) . filter (\case (Drop _, _, _) -> False; _ -> True)
        letReceives = foldr ((\ (v, r) c -> Let v r c) . (\(_,v,r) -> (v,r)))

generateReceiveCode :: (Semigroup b, IsString b, Show a) => (a, CallArg ty) -> (CallArg ty, b, TaskExpr ty)
generateReceiveCode (idx, a@(Arg r)) = (a, "var_" <> show idx, ReceiveData r)
generateReceiveCode (idx, a@(Drop (Left r))) = (a, "_var_" <> show idx, ReceiveData r)
generateReceiveCode (idx, a@(Drop (Right e))) = (a, "_var_" <> show idx, e)
generateReceiveCode (idx, a@(Converted e)) = (a, "var_" <> show idx, e)

funReceives :: FusFunction f ty -> [Com 'Recv ty]
funReceives = 
    \case
        (PureFusable vars _ _) -> extract vars
        (STFusable r vars _ _ _) -> r : extract vars
    where 
        extract = mapMaybe 
                    (\case 
                        (Arg r) -> Just r
                        (Drop (Left r)) -> Just r
                        (Drop _) -> Nothing
                        (Converted _) -> Nothing)

data FusedFun ty 
    = FusedFun (FusedFunction ty) (TaskExpr ty)

genFusedFun :: FusedFun ty -> TaskExpr ty
genFusedFun (FusedFun fun ct) = loop fun $ genFun' ct fun

fuseFuns :: FusableFunction ty -> FusableFunction ty -> FusableFunction ty
fuseFuns = undefined