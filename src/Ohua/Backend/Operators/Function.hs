module Ohua.Backend.Operators.Function where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)

import qualified Data.List.NonEmpty as NE


data Function 
    = Pure
        QualifiedBinding
        [Either Recv TaskExpr] -- call args
        -- Assumption: if there is no one that needs this result, then this should not be computed
        --             in the first place!
        Binding -- out
    | ST
        QualifiedBinding
        Recv -- state
        [Either Recv TaskExpr] -- call args
        (Maybe Binding) -- state out
        (Maybe Binding) -- out

data FusableFunction 
    = PureFusable
        [Either Recv TaskExpr]  -- data receive
        ([Binding] -> TaskExpr -> TaskExpr) -- application
        Send -- send result
    | STFusable
        Recv -- state receive
        [Either Recv TaskExpr]  -- data receive
        (NonEmpty Binding -> TaskExpr -> TaskExpr) -- application
        (Maybe Send) -- send result
        (Maybe (Binding -> Send)) -- send state

gen :: FusableFunction -> TaskExpr
gen = \case
    (PureFusable receives app send) ->
        varsAndReceives receives $ 
        app (bnds receives) $
        (\(Emit c d) -> Send c d) send
    (STFusable stateRecv receives app sendRes sendState) ->
        varsAndReceives (Left stateRecv : receives) $
        app (bndsNE $ Left stateRecv :| receives) $
        foldr (\(Emit ch d) c -> Stmt (Send ch d) c) (Lit UnitLit) $ 
        catMaybes [sendRes, "var_0" <$> sendState]
    where
        bnds = map (("var_" <>) . show . fst) . zip [0..] 
        bndsNE = NE.map (("var_" <>) . show . fst) . NE.zip [0..] 
        varsAndReceives rcvs cont = 
            foldr (\(v,r) c -> Let v r c) cont $
            map generateReceiveCode $
            zip [0..] rcvs
        generateReceiveCode (idx, Left (Recv cidx bnd)) = ("var_" <> show idx, Receive cidx bnd)
        generateReceiveCode (idx, Right e) = ("var_" <> show idx, e)

fun :: Function -> FusableFunction
fun = \case 
    (Pure funRef callArgs out) ->
        PureFusable
            callArgs
            (\args cont -> 
                Let "result" (Apply $ Stateless funRef $ map Var args)
                    cont)
            (out `Emit` "result")
    (ST funRef stateVar@(Recv _ stateBnd) callArgs stateOut out) ->
        STFusable
            stateVar
            callArgs
            (\args cont -> 
                Let "result" (Apply $ Stateful (NE.head args) funRef $ map Var $ NE.tail args)
                    cont)
            ((`Emit` "result") <$> out)
            (Emit <$> stateOut)
