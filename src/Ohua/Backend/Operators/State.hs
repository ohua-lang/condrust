module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang

import qualified Text.Show


type DataSizeInput = Com 'Recv
type StateInput = Com 'Recv
type StateOutput = Com 'Channel

type StateBnd = Binding

-- FIXME remove the functions from the type and derive the type classes normally
data STCLangSMap = 
    STCLangSMap
        (TaskExpr -> TaskExpr) -- init
        (TaskExpr -> TaskExpr) -- ctxt loop
        StateInput -- state receive
        StateOutput -- state emission
    deriving (Generic)

instance Eq STCLangSMap where
    (STCLangSMap _ _ inp out) == (STCLangSMap _ _ inp' out') = inp == inp' && out == out'

instance Hashable STCLangSMap where
    hashWithSalt s (STCLangSMap _ _ inp out) = s `hashWithSalt` inp `hashWithSalt` out

instance Show STCLangSMap where
    show (STCLangSMap _ _ inp out) = "STCLangSMap { state input: " <> show inp <> " , state output: " <> show out <> " }"

genSTCLangSMap :: STCLangSMap -> TaskExpr
genSTCLangSMap (STCLangSMap init ctxtLoop stateReceive emit) = 
    init $
    Stmt
    (
        ctxtLoop $
            Stmt (ReceiveData stateReceive) $
            Lit UnitLit
    ) $
    Let "s" (ReceiveData stateReceive)
        $ SendData $ SSend emit "s"

mkSTCLangSMap :: DataSizeInput -> StateInput -> StateOutput -> STCLangSMap
mkSTCLangSMap sizeInput = STCLangSMap init ctxtLoop
    where
        init :: TaskExpr -> TaskExpr
        init c =  
            Let "num" (ReceiveData sizeInput) $
            Let "toDrop" (Decrement "num") $
            Let "drops" (Generate "toDrop" UnitLit) c

        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "drops" 
