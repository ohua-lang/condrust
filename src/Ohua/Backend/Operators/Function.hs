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

data FusFunction pout sin ty
    = PureFusable
        [CallArg ty]  -- data receive
        QualifiedBinding
        (pout (Com 'Channel ty)) -- send result
    | STFusable
        (sin ty) -- state receive
        [CallArg ty]  -- data receive
        QualifiedBinding
        (Maybe (Com 'Channel ty)) -- send result
        (Maybe (Com 'Channel ty)) -- send state
    | IdFusable
        (CallArg ty)
        (pout (Com 'Channel ty))
    deriving (Generic)

type FusableFunction ty = FusFunction Identity (Com 'Recv) ty
type FusedFunction ty = FusFunction Maybe CallArg ty

deriving instance Hashable (FusableFunction ty)
deriving instance Eq (FusableFunction ty)

toFuseFun :: FusableFunction ty -> FusedFunction ty
toFuseFun (PureFusable recvs qb (Identity out)) = PureFusable recvs qb $ Just out
toFuseFun (STFusable a b c d e) = STFusable (Arg a) b c d e
toFuseFun (IdFusable recv (Identity out)) = IdFusable recv $ Just out

genFun :: FusableFunction ty -> TaskExpr ty
genFun fun = loop (funReceives fun) $ (\f -> genFun' (genSend f) f) $ toFuseFun fun

funReceives :: FusableFunction ty -> [Com 'Recv ty]
funReceives (PureFusable vars _ _)   = extractAll vars
funReceives (STFusable r vars _ _ _) = r : extractAll vars
funReceives (IdFusable r _) = extractAll [r]

loop :: [Com 'Recv ty] -> TaskExpr ty -> TaskExpr ty
loop [] c = c
loop _  c = EndlessLoop c

genSend :: forall ty.FusedFunction ty -> TaskExpr ty
genSend = \case
    (PureFusable _ _ o) -> pureOut o
    (STFusable stateRecv receives _ sendRes sendState) ->
        let varsAndReceives =
              NE.zipWith (curry generateReceiveCode) [0 ..] $ stateRecv :| receives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
        in foldr Stmt (Lit UnitLit) $
            catMaybes [ SendData . (\o@(SChan b) -> o `SSend` b) <$> sendRes
                      , SendData . (`SSend` stateArg) <$> sendState
                      ]
    (IdFusable _ o) -> pureOut o
    where
      pureOut :: Maybe (Com 'Channel ty) -> TaskExpr ty
      pureOut = maybe (Lit UnitLit) (\out@(SChan b) -> SendData $ SSend out b)

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
        let varsAndReceives =
              NE.zipWith (curry generateReceiveCode) [0 ..] $ stateRecv :| receives
            callArgs = getCallArgs $ NE.tail varsAndReceives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
            call = (Apply $ Stateful (Var stateArg) app callArgs)
        in flip letReceives (toList varsAndReceives) $
            maybe
                (Stmt call ct)
                (\(SChan b) -> Let b call ct)
                sendRes
    (IdFusable i o) ->
        let varsAndReceives = zipWith (curry generateReceiveCode) [0 ..] [i]
        in letReceives ct varsAndReceives 
    where
        getCallArgs =
          map (\(_,v,_) -> Var v) .
          filter (\case (Drop _, _, _) -> False; _ -> True)
        letReceives = foldr ((\ (v, r) c -> Let v r c) . (\(_,v,r) -> (v,r)))

generateReceiveCode :: (Semigroup b, IsString b, Show a) => (a, CallArg ty) -> (CallArg ty, b, TaskExpr ty)
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

fuseFuns :: FusableFunction ty -> FusableFunction ty -> FusableFunction ty
fuseFuns = undefined
