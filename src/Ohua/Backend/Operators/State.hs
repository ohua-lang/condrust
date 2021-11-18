{-# LANGUAGE DataKinds, ScopedTypeVariables, DeriveAnyClass #-}
module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang


type DataSizeInput ty = Com 'Recv ty
type StateInput ty = Com 'Recv ty
type StateOutput ty = Com 'Channel ty

type StateBnd = Binding

data Fuse = Fusable | Unfusable deriving (Generic, Eq, Show)

data STCLangSMap :: Fuse -> Type -> Type where
  STCLangSMap ::
    -- | Count until state emission
    DataSizeInput ty ->
    -- | state receive
    StateInput ty ->
    -- | state emission
    StateOutput ty -> STCLangSMap 'Unfusable ty
  FusableSTCLangSMap ::
    -- | state receive
    StateInput ty ->
    -- | state emission
    StateOutput ty -> STCLangSMap 'Fusable ty

deriving instance Eq (STCLangSMap fusable ty)
deriving instance Show (STCLangSMap fusable ty)

instance Hashable (STCLangSMap fuse ty) where
    hashWithSalt s (STCLangSMap cInp sIn sOut) = s `hashWithSalt` cInp `hashWithSalt` sIn `hashWithSalt` sOut
    hashWithSalt s (FusableSTCLangSMap sInp sOut) = s `hashWithSalt` sInp `hashWithSalt` sOut

genSTCLangSMap :: forall ty. STCLangSMap 'Unfusable ty -> TaskExpr ty
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
        init :: TaskExpr ty -> TaskExpr ty
        init c =
            Let "num" (ReceiveData sizeInput) $
            Let "toDrop" (Decrement "num") c
            --Let "drops" (Generate "toDrop" UnitLit) c
            -- TODO: Verify correctness

        ctxtLoop :: TaskExpr ty -> TaskExpr ty
        ctxtLoop = Repeat $ Left "toDrop"

