module Ohua.Backend.Operators.Function where

import Ohua.Prelude

import Ohua.Backend.Lang as L


data Function 
    = Pure
        FunRef
        [Either Binding Lit] -- call args
        -- Assumption: if there is no one that needs this result, then this should not be computed
        --             in the first place!
        Binding -- out
    | ST
        FunRef
        Binding -- state
        [Either (Binding, Int) Lit] -- call args
        Maybe Binding -- state out
        Maybe Binding -- out

data FusableFunction 
    = PureFusable
        [Either Com Lit]  -- args
        ([Binding] -> TaskExpr -> TaskExpr) -- application
        Com -- send result
    | STFusable
        [Either Com Lit]  -- args
        (NonEmpty Binding -> TaskExpr -> TaskExpr) -- application
        Com -- send state
        Com -- send result

fun :: CompM m => Function -> FusableFunction
fun (Pure funRef callArgs out) = 
    PureFusable
        (map generateReceiveCode callArgs)
        (\args cont -> 
            Let "result" (Apply $ Stateless funRef $ map Var args)
                cont)
        (Com out $ Send out "result")
    
fun (ST funRef stateVar callArgs stateOut out) = undefined
    STFusable
        (map generateReceiveCode $ stateVar : callArgs)
        (\args cont -> 
            Let "result" (Apply $ Stateful stateVar funRef $ map Var args)
                cont)
        (Com out $ Send out "result")
        (Com stateOut $ Send stateOut stateVar)

generateReceiveCode (Left (bnd, idx)) = Com bnd $ Receive idx bnd
generateReceiveCode (Right l) = l
