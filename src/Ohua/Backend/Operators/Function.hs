module Ohua.Backend.Operators.Function where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)

import qualified Data.List.NonEmpty as NE


-- FIXME Seems to be like this: Arg Recv (Either Recv TaskExpr) | Drop (Either Recv TaskExpr)
data CallArg 
    = Arg Recv | Drop (Either Recv TaskExpr) | Converted TaskExpr
    deriving (Eq, Show, Generic)

instance Hashable CallArg

-- TODO:
--   1) no functions in data type -> done
--   2) deriving instance ... -> done
--   3) take over OutData
--   4) implement dispatch and destructuring via nth and ...?
--   5) certainly this definition of a function really is no different
--      from the newly defined DFLang! (one more reason to move this into common.)

data FusableFunction 
    = PureFusable
        [CallArg]  -- data receive
        QualifiedBinding
        Binding -- send result
    | STFusable
        Recv -- state receive
        [CallArg]  -- data receive
        QualifiedBinding
        (Maybe Binding) -- send result
        (Maybe Binding) -- send state
    deriving (Show, Eq, Generic)


instance Hashable FusableFunction 

genFun :: FusableFunction -> TaskExpr
genFun fun = loop $ genFun' fun
    where
        loop c = if null $ funReceives fun
                then c
                else EndlessLoop c

genFun' :: FusableFunction -> TaskExpr
genFun' = \case
    (PureFusable receives app out) ->
        let varsAndReceives = zipWith (curry generateReceiveCode) [0 ..] receives
            callArgs = getCallArgs varsAndReceives
        in flip letReceives varsAndReceives $
            Let "result" (Apply $ Stateless app callArgs) $
            Send out "result"
    (STFusable stateRecv receives app sendRes sendState) ->
        let varsAndReceives = NE.zipWith (curry generateReceiveCode) [0 ..] $ Arg stateRecv :| receives
            callArgs = getCallArgs $ NE.tail varsAndReceives
            stateArg = (\(_,v,_) -> v) $ NE.head varsAndReceives
        in flip letReceives (toList varsAndReceives) $
            Let "result" (Apply $ Stateful stateArg app callArgs) $
            foldr Stmt (Lit UnitLit) $ 
            catMaybes [(`Send` "result") <$> sendRes, (`Send` stateArg) <$> sendState]
    where
        getCallArgs = map (\(_,v,_) -> Var v) . filter (\case (Drop _, _, _) -> False; _ -> True)
        letReceives = foldr ((\ (v, r) c -> Let v r c) . (\(_,v,r) -> (v,r)))
        generateReceiveCode (idx, a@(Arg (Recv cidx bnd))) = (a, "var_" <> show idx, Receive cidx bnd)
        generateReceiveCode (idx, a@(Drop (Left (Recv cidx bnd)))) = (a, "_var_" <> show idx, Receive cidx bnd)
        generateReceiveCode (idx, a@(Drop (Right e))) = (a, "_var_" <> show idx, e)
        generateReceiveCode (idx, a@(Converted e)) = (a, "var_" <> show idx, e)

funReceives :: FusableFunction -> [Binding]
funReceives = 
    \case
        (PureFusable vars _ _) -> extract vars
        (STFusable (Recv _ bnd) vars _ _ _) -> bnd : extract vars
    where 
        extract = mapMaybe 
                    (\case 
                        (Arg (Recv _ bnd)) -> Just bnd
                        (Drop (Left (Recv _ bnd))) -> Just bnd
                        (Drop _) -> Nothing
                        (Converted _) -> Nothing)
        