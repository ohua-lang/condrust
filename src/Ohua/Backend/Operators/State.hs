module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang


type DataSizeInput = Binding
type FoldFun = FunRef
type StateOutput = Binding

type StateBnd = Binding

data STCLangSMap = 
    STCLangSMap
        (TaskExpr -> TaskExpr) -- init
        (TaskExpr -> TaskExpr) -- ctxt loop
        (StateBnd -> TaskExpr) -- state receive
        (StateBnd -> TaskExpr) -- state emission

genSTCLangSMap :: STCLangSMap -> TaskExpr
genSTCLangSMap (STCLangSMap init ctxtLoop stateReceive emit) = 
    init $
    Stmt 
    (
        ctxtLoop $
            Stmt (stateReceive "state") $
            Lit UnitLit
    ) $
    Let "state" stateReceive
        $ emit "state"

mkSTCLangSMap :: DataSizeInput -> StateOutput -> STCLangSMap
mkSTCLangSMap sizeInput stateOutput = 
    STCLangSMap init ctxtLoop stateReceiveCode emit
    where
        init :: Binding -> TaskExpr -> TaskExpr
        init sizeInput =  
            Let "num" (Receive 0 sizeInput) $
            Let "toDrop" (Decrement "num") $
            Let "drops" (Generate "toDrop" UnitLit)

        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat "drops" 
                
        stateReceiveCode :: StateBnd -> TaskExpr
        stateReceiveCode = Receive 0

        emit :: TaskExpr
        emit = Send stateOutput

