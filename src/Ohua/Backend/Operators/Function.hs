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

data FusableFunction ty
    = PureFusable
        [CallArg ty]  -- data receive
        QualifiedBinding
        (Com 'Channel ty) -- send result
    | STFusable
        (Com 'Recv ty) -- state receive
        [CallArg ty]  -- data receive
        QualifiedBinding
        (Maybe (Com 'Channel ty)) -- send result
        (Maybe (Com 'Channel ty)) -- send state
    deriving (Eq,Generic)

instance Hashable (FusableFunction ty)

genFun :: FusableFunction ty -> TaskExpr ty
genFun fun = loop $ genFun' fun
    where
        loop c = if null $ funReceives fun
                then c
                else EndlessLoop c

genFun' :: FusableFunction ty -> TaskExpr ty
genFun' = \case
    (PureFusable receives app out) ->
        let varsAndReceives = zipWith (curry generateReceiveCode) [0 ..] receives
            callArgs = getCallArgs varsAndReceives
        in flip letReceives varsAndReceives $
            Let "result" (Apply $ Stateless app callArgs) $
            SendData $ SSend out "result"
    (STFusable stateRecv receives app sendRes sendState) ->
        let varsAndReceives = NE.zipWith (curry generateReceiveCode) [0 ..] $ Arg stateRecv :| receives
            callArgs = getCallArgs $ NE.tail varsAndReceives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
        in flip letReceives (toList varsAndReceives) $
            Let "result" (Apply $ Stateful (Var stateArg) app callArgs) $
            foldr Stmt (Lit UnitLit) $ 
            catMaybes [SendData . (`SSend` "result") <$> sendRes, SendData . (`SSend` stateArg) <$> sendState]
    where
        getCallArgs = map (\(_,v,_) -> Var v) . filter (\case (Drop _, _, _) -> False; _ -> True)
        letReceives = foldr ((\ (v, r) c -> Let v r c) . (\(_,v,r) -> (v,r)))
        generateReceiveCode (idx, a@(Arg r)) = (a, "var_" <> show idx, ReceiveData r)
        generateReceiveCode (idx, a@(Drop (Left r))) = (a, "_var_" <> show idx, ReceiveData r)
        generateReceiveCode (idx, a@(Drop (Right e))) = (a, "_var_" <> show idx, e)
        generateReceiveCode (idx, a@(Converted e)) = (a, "var_" <> show idx, e)

funReceives :: FusableFunction ty -> [Com 'Recv ty]
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
        