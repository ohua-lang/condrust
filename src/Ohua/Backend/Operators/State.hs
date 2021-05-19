{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass #-}
module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang


type DataSizeInput ty = Com 'Recv ty
type StateInput ty = Com 'Recv ty
type StateOutput ty = Com 'Channel ty

type StateBnd = Binding

data STCLangSMap ty = 
    STCLangSMap
        (DataSizeInput ty)
        (StateInput ty) -- state receive
        (StateOutput ty) -- state emission
    deriving (Generic, Eq)

deriving instance Hashable (STCLangSMap ty)

genSTCLangSMap :: forall ty. STCLangSMap ty -> TaskExpr ty
genSTCLangSMap (STCLangSMap sizeInput stateReceive emit) = 
    init $
    Stmt
    (
        ctxtLoop $
            Stmt (ReceiveData stateReceive) $
            Lit UnitLit
    ) $
    Let "s" (ReceiveData stateReceive)
        $ SendData $ SSend emit "s"
    where
        init :: TaskExpr ty -> TaskExpr ty
        init c =  
            Let "num" (ReceiveData sizeInput) $
            Let "toDrop" (Decrement "num") c
            --Let "drops" (Generate "toDrop" UnitLit) c
            -- TODO: Verify correctness

        ctxtLoop :: TaskExpr ty -> TaskExpr ty
        ctxtLoop = Repeat $ Left "toDrop" 


mkSTCLangSMap :: forall ty. DataSizeInput ty -> StateInput ty -> StateOutput ty -> STCLangSMap ty
mkSTCLangSMap = STCLangSMap
