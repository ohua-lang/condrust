{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module Ohua.Backend.Operators.State where

import Ohua.Commons.Prelude

import Ohua.Backend.Lang


type DataSizeInput embExpr ty = Com 'Recv embExpr ty
type StateInput embExpr ty = Com 'Recv embExpr ty
type StateOutput embExpr ty = Com 'Channel embExpr ty

type StateBnd = Binding

data Fuse = Fusable | Unfusable deriving (Generic, Eq, Show)

data STCLangSMap :: Fuse -> Type -> Type -> Type where
  STCLangSMap ::
    -- | Count until state emission
    DataSizeInput embExpr ty ->
    -- | state receive
    StateInput embExpr ty ->
    -- | state emission
    StateOutput embExpr ty -> STCLangSMap 'Unfusable embExpr ty
  FusableSTCLangSMap ::
    -- | state receive
    StateInput embExpr ty ->
    -- | state emission
    StateOutput embExpr ty -> STCLangSMap 'Fusable embExpr ty

deriving instance Eq (STCLangSMap fusable embExpr ty)
deriving instance Show (STCLangSMap fusable embExpr ty)

instance Hashable (STCLangSMap fuse embExpr ty) where
    hashWithSalt s (STCLangSMap cInp sIn sOut) = s `hashWithSalt` cInp `hashWithSalt` sIn `hashWithSalt` sOut
    hashWithSalt s (FusableSTCLangSMap sInp sOut) = s `hashWithSalt` sInp `hashWithSalt` sOut

genSTCLangSMap :: forall embExpr ty. STCLangSMap 'Unfusable embExpr ty -> TaskExpr embExpr ty
genSTCLangSMap (STCLangSMap sizeInput stateReceive emit) =
    init $
    Stmt
    (
        ctxtLoop $
            Stmt (ReceiveData stateReceive) $
            Lit UnitLit
    ) $
    Let "s" (ReceiveData stateReceive)
        $ SendData $ SSend emit $ Left "s"
    where
        init :: TaskExpr embExpr ty -> TaskExpr embExpr ty
        init c =
            Let "num" (ReceiveData sizeInput) $
            Let "toDrop" (Decrement "num") c
            --Let "drops" (Generate "toDrop" UnitLit) c
            -- TODO: Verify correctness

        ctxtLoop :: TaskExpr embExpr ty -> TaskExpr embExpr ty
        ctxtLoop = Repeat $ Left "toDrop"

