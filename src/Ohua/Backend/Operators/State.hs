{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module Ohua.Backend.Operators.State where

import Ohua.Commons.Prelude

import Ohua.Backend.Lang


type DataSizeInput embExpr annot ty = Com 'Recv embExpr annot ty
type StateInput embExpr annot ty = Com 'Recv embExpr annot ty
type StateOutput embExpr annot ty = Com 'Channel embExpr annot ty

type StateBnd = Binding

data Fuse = Fusable | Unfusable deriving (Generic, Eq, Show)

data STCLangSMap :: Fuse -> Type -> Type ->Type -> Type where
  STCLangSMap ::
    -- | Count until state emission
    DataSizeInput embExpr annot ty ->
    -- | state receive
    StateInput embExpr annot ty ->
    -- | state emission
    StateOutput embExpr annot ty -> STCLangSMap 'Unfusable embExpr annot ty
  FusableSTCLangSMap ::
    -- | state receive
    StateInput embExpr annot ty ->
    -- | state emission
    StateOutput embExpr annot ty -> STCLangSMap 'Fusable embExpr annot ty

deriving instance Eq (STCLangSMap fusable embExpr annot ty)
deriving instance Show (STCLangSMap fusable embExpr annot ty)

instance Hashable (STCLangSMap fuse embExpr annot ty) where
    hashWithSalt s (STCLangSMap cInp sIn sOut) = s `hashWithSalt` cInp `hashWithSalt` sIn `hashWithSalt` sOut
    hashWithSalt s (FusableSTCLangSMap sInp sOut) = s `hashWithSalt` sInp `hashWithSalt` sOut

genSTCLangSMap :: forall embExpr annot ty. STCLangSMap 'Unfusable embExpr annot ty -> TaskExpr embExpr annot ty
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
        init :: TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty
        init c =
            Let "num" (ReceiveData sizeInput) $
            Let "toDrop" (Decrement "num") c
            --Let "drops" (Generate "toDrop" UnitLit) c
            -- TODO: Verify correctness

        ctxtLoop :: TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty
        ctxtLoop = Repeat $ Left "toDrop"

