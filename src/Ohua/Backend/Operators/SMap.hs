{-# LANGUAGE DeriveAnyClass #-}
module Ohua.Backend.Operators.SMap
  ( smapFun
  , collect
  , Op()
  , fuse
  , gen, gen'
  , getInput
  , DataIn(..)
  ) where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (Var)
import qualified Ohua.Backend.Lang as L ( TaskExpr( Var ) )
import Ohua.Backend.Operators.Common

type Input = Com 'Recv
type DataOut ty = Maybe (Com 'Channel ty)
type CtrlOut ty = [Com 'Channel ty]
type CollectOut ty = [Com 'Channel ty]

type SizeInput = Com 'Recv
type DataInput = Com 'Recv
type CollectedOutput = Com 'Channel

data DataIn ty = Var Binding | Receive (Input ty) | Expr (TaskExpr ty)
  deriving (Generic,Eq)

deriving instance Hashable (DataIn ty)

-- This could easily be turned into something more general.
data Op ty
  = SMap (DataIn ty) (DataOut ty) (CtrlOut ty) (CollectOut ty)
  | Collect (DataIn ty) (SizeInput ty) (CollectedOutput ty)
  deriving (Generic,Eq)

deriving instance Hashable (Op ty)

smapFun :: DataIn ty -> DataOut ty -> CtrlOut ty -> CollectOut ty -> Op ty
smapFun = SMap

collect :: DataInput ty -> SizeInput ty -> CollectedOutput ty -> Op ty
collect dataInput = Collect (Receive dataInput)

getInput :: Op ty -> Maybe (Input ty)
getInput (SMap (Receive r) _ _ _) = Just r
getInput (Collect (Receive r) _ _) = Just r
getInput _ = Nothing

gen :: Op ty -> TaskExpr ty
gen = EndlessLoop . gen'

gen' :: Op ty -> TaskExpr ty
gen' (SMap input dataOut ctrlOut collectOut) =
  Let "data" (genInput input) $
  Let "hasSize" (HasSize "data") $
    Cond (L.Var "hasSize")
    -- known size
    (
      Let "size" (Size "data") $
      g collectOut (\c -> Stmt $ SendData $ SSend c $ Left "size") $
      g ctrlOut (\c -> Let "ctrl" (Tuple (Right $ BoolLit True) (Left "size")) .
                       Stmt (SendData $ SSend c $ Left "ctrl") ) $
      f dataOut (\dOut -> ForEach "d" "data" .
                          Stmt (SendData $ SSend dOut $ Left "d"))
      $ Lit UnitLit
    )
    -- unknown size --> generator-style
    (
      Let "size" (Lit $ NumericLit 0) $
      Stmt
      (ForEach "d" "data" $
        f dataOut (\dOut -> Stmt $ SendData $ SSend dOut $ Left "d") $
        g ctrlOut
        (\c -> Let "ctrl" (Tuple (Right $ BoolLit False) (Right $ NumericLit 1)) .
               Stmt (SendData $ SSend c $ Left "ctrl"))
        $ Assign "size" $ Increment "size") $
      g collectOut (\c -> Stmt $ SendData $ SSend c $ Left "size") $
      g ctrlOut (\c -> Let "ctrl" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) .
                         Stmt (SendData $ SSend c $ Left "ctrl"))
      $ Lit UnitLit
    )
  where
    f o e cont = case o of
                   Just c -> (e c) cont
                   Nothing -> cont
    g o e cont = case o of
                   [] -> cont
                   (x:xs) -> (e x) (g xs e cont)

gen' (Collect dataInput sizeInput collectedOutput) =
  Let "num" (ReceiveData sizeInput) $
  Let "collection" (ListOp Create) $
  Stmt
  (
    Repeat (Left "num") $
      Let "data" (genInput dataInput) $
      ListOp $ Append "collection" $ L.Var "data"
  ) $
  SendData $ SSend collectedOutput $ Left "collection"


genInput :: DataIn ty -> TaskExpr ty
genInput (Var bnd)   = L.Var bnd
genInput (Receive r) = ReceiveData r
genInput (Expr t)    = t

fuse :: Binding -> Op ty -> Op ty
fuse var (SMap _ a b c)  = SMap (Var var) a b c
fuse var (Collect _ a b) = Collect (Var var) a b
