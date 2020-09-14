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

-- TODO
fuse = undefined

genSTCLangSMap :: STCLangSMap -> TaskExpr
genSTCLangSMap (STCLangSMap init ctxtLoop stateReceive emit) = 
    init $
    Stmt 
    (
        ctxtLoop $
            Stmt (stateReceive "state") $
            Lit UnitLit
    ) $
    Let "s" (stateReceive "state")
        $ emit "s"

mkSTCLangSMap :: DataSizeInput -> StateOutput -> STCLangSMap
mkSTCLangSMap sizeInput stateOutput = 
    STCLangSMap init ctxtLoop stateReceiveCode emit
    where
        init :: TaskExpr -> TaskExpr
        init c =  
            Let "num" (Receive 0 sizeInput) $
            Let "toDrop" (Decrement "num") $
            Let "drops" (Generate "toDrop" UnitLit) c

        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "drops" 
                
        stateReceiveCode :: StateBnd -> TaskExpr
        stateReceiveCode = Receive 0

        emit :: StateBnd -> TaskExpr
        emit = Send stateOutput

