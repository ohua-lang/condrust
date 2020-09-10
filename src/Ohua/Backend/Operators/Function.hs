module Ohua.Backend.Operators.Function where

import Ohua.Prelude

import Ohua.Backend.Lang as L


data Function 
    = Pure
        FunRef
        [Either Recv Lit] -- call args
        -- Assumption: if there is no one that needs this result, then this should not be computed
        --             in the first place!
        Send -- out
    | ST
        FunRef
        Binding -- state
        [Either (Binding, Int) Lit] -- call args
        Maybe Binding -- state out
        Maybe Binding -- out

data DataSource 
    = Recv 
    | TaskExpr 

data FusableFunction 
    = PureFusable
        [DataSource]  -- data receive
        ([Binding] -> TaskExpr -> TaskExpr) -- application
        (Maybe Send) -- send result
    | STFusable
        [DataSource]  -- data receive
        (NonEmpty Binding -> TaskExpr -> TaskExpr) -- application
        (Maybe Send) -- send state
        (Maybe Send) -- send result

gen :: FusableFunction
gen f = 
    case f of
        (PureFusable receives app send) ->
            varsAndReceives $ 
            app (bnds receives) $
            maybe (Lit UnitLit) (\(Emit c d) -> Send c d) send
        (STFusable receives app sendState sendRes) ->
            varsAndReceives $
            app (bnds $ NE.toList receives) $
            foldr (\(Emit ch d) c -> Stmt (Send ch d) c) (Lit UnitLit) $ 
            catMaybe [sendState, sendRes]
    where
        bnds = map (("var_" <>) . show . fst) . zip [0..] 
        varsAndReceives cont = 
            foldr (\(v,r) c -> Let v r c) cont $
            map (curry generateReceiveCode) $ 
            zip [0..] receives
        generateReceiveCode idx (Recv cidx bnd) = ("var_" <> show idx, Receive cidx bnd)
        generateReceiveCode idx e = ("var_" <> show idx, e)

fun :: Function -> FusableFunction
fun (Pure funRef callArgs out) = 
    PureFusable
        (map dataSource callArgs)
        (\args cont -> 
            Let "result" (Apply $ Stateless funRef $ map Var args)
                cont)
        (`Emit` "result" <$> out)
fun (ST funRef stateVar callArgs stateOut out) = undefined
    STFusable
        (map generateReceiveCode $ stateVar : callArgs)
        (\args cont -> 
            Let "result" (Apply $ Stateful stateVar funRef $ map Var args)
                cont)
        (`Emit` "result" <$> out)
        (`Emit` stateVar <$> out)

dataSource (Left (bnd, idx)) = Recv idx bnd
dataSource (Right l) = l
