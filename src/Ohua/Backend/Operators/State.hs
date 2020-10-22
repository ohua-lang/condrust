{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang


type DataSizeInput ty = Com 'Recv ty
type StateInput ty = Com 'Recv ty
type StateOutput ty = Com 'Channel ty

type StateBnd = Binding

-- FIXME remove the functions from the type and derive the type classes normally
data STCLangSMap ty = 
    STCLangSMap
        (TaskExpr ty -> TaskExpr ty) -- init
        (TaskExpr ty -> TaskExpr ty) -- ctxt loop
        (StateInput ty) -- state receive
        (StateOutput ty) -- state emission
    deriving (Generic)

instance Eq (STCLangSMap ty) where
    (STCLangSMap _ _ inp out) == (STCLangSMap _ _ inp' out') = inp == inp' && out == out'

instance Hashable (STCLangSMap ty) where
    hashWithSalt s (STCLangSMap _ _ inp out) = s `hashWithSalt` inp `hashWithSalt` out

genSTCLangSMap :: STCLangSMap ty -> TaskExpr ty
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

mkSTCLangSMap :: forall ty. DataSizeInput ty -> StateInput ty -> StateOutput ty -> STCLangSMap ty
mkSTCLangSMap sizeInput = STCLangSMap init ctxtLoop
    where
        init :: TaskExpr ty -> TaskExpr ty
        init c =  
            Let "num" (ReceiveData sizeInput) $
            Let "toDrop" (Decrement "num") $
            Let "drops" (Generate "toDrop" UnitLit) c

        ctxtLoop :: TaskExpr ty -> TaskExpr ty
        ctxtLoop = Repeat $ Left "drops" 
