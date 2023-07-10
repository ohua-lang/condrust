module Ohua.Types.Casts where

import Universum

import qualified Ohua.Types.Resolved.Types as Res (VarType(..), FunType(..))
import qualified Ohua.Types.Unresolved.Types as URes (VarType(..), FunType(..))

import Data.Maybe

toResType :: URes.VarType ty -> Maybe (Res.VarType ty)
toResType URes.TypeNat = Just Res.TypeNat
toResType URes.TypeBool = Just Res.TypeBool
toResType URes.TypeUnit = Just Res.TypeUnit
toResType URes.TypeString = Just Res.TypeString
toResType (URes.TypeList l) = Res.TypeList <$> toResType l
toResType (URes.Type t) = Just $ Res.Type t
toResType (URes.TupleTy xs) = Res.TupleTy <$> (sequence $ map toResType xs)
toResType (URes.TypeFunction f) = Res.TypeFunction <$> toResFunType f
toResType URes.TypeVar = Nothing

toResFunType :: URes.FunType ty -> Maybe (Res.FunType ty)
toResFunType (URes.FunType ins out) = Res.FunType <$> (sequence $ map toResType ins) <*> toResType out
toResFunType (URes.STFunType sin ins out) =
  Res.STFunType <$>
  toResType sin <*>
  (sequence $ map toResType ins) <*>
  toResType out

fromResType :: Res.VarType ty -> URes.VarType ty
fromResType Res.TypeNat = URes.TypeNat
fromResType Res.TypeBool = URes.TypeBool
fromResType Res.TypeUnit = URes.TypeUnit
fromResType Res.TypeString = URes.TypeString
fromResType (Res.TypeList l) = URes.TypeList $ fromResType l
fromResType (Res.Type t) = URes.Type t
fromResType (Res.TupleTy xs) = URes.TupleTy $ map fromResType xs
fromResType (Res.TypeFunction f) = URes.TypeFunction $ fromResFunType f

fromResFunType :: Res.FunType ty -> URes.FunType ty
fromResFunType (Res.FunType ins out) = URes.FunType (map fromResType ins) $ fromResType out
fromResFunType (Res.STFunType sin ins out) =
  URes.STFunType
  (fromResType sin)
  (map fromResType ins)
  (fromResType out)
