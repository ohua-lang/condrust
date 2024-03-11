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

import Ohua.Commons.Prelude

import Ohua.Backend.Lang hiding (Var)
import qualified Ohua.Backend.Lang as L ( TaskExpr( Var ) )
import Ohua.Backend.Operators.Common

type Input = Com 'Recv
type DataOut embExpr annot ty = Maybe (Com 'Channel embExpr annot ty)
type CtrlOut embExpr annot ty = [Com 'Channel embExpr annot ty]
type CollectOut embExpr annot ty = [Com 'Channel embExpr annot ty]

type SizeInput = Com 'Recv
type DataInput = Com 'Recv
type CollectedOutput = Com 'Channel

data DataIn embExpr annot ty = Var Binding | Receive (Input embExpr annot ty) | Expr (TaskExpr embExpr annot ty)
  deriving (Generic,Eq, Show)

deriving instance Hashable (DataIn embExpr annot ty)


-- This could easily be turned into something more general.
data Op embExpr annot ty
  = SMap (DataIn embExpr annot ty) (DataOut embExpr annot ty) (CtrlOut embExpr annot ty) (CollectOut embExpr annot ty)
  | Collect (DataIn embExpr annot ty) (SizeInput embExpr annot ty) (CollectedOutput embExpr annot ty)
  deriving (Generic,Eq)

deriving instance Hashable (Op embExpr annot ty)

deriving instance Show (Op embExpr annot ty)

smapFun :: DataIn embExpr annot ty -> DataOut embExpr annot ty -> CtrlOut embExpr annot ty -> CollectOut embExpr annot ty -> Op embExpr annot ty
smapFun = SMap

collect :: DataInput embExpr annot ty -> SizeInput embExpr annot ty -> CollectedOutput embExpr annot ty -> Op embExpr annot ty
collect dataInput = Collect (Receive dataInput)

getInput :: Op embExpr annot ty -> Maybe (Input embExpr annot ty)
getInput (SMap (Receive r) _ _ _) = Just r
getInput (Collect (Receive r) _ _) = Just r
getInput _ = Nothing

gen :: Op embExpr annot ty -> TaskExpr embExpr annot ty
gen smap = case getInput smap of
             Just _ -> EndlessLoop $ gen' smap
             _ -> gen' smap

gen' :: Op embExpr annot ty -> TaskExpr embExpr annot ty
gen' (SMap input dataOut ctrlOut collectOut) =
  Let "data" (genInput input) $
  Let "hasSize" (HasSize "data") $
    Cond (L.Var "hasSize")
    -- known size
    (
      Let "size" (Size "data") $
      g collectOut (\c -> Stmt $ SendData $ SSend c $ Left "size") $
      g ctrlOut (\c -> Let "ctrl" (ctrlTuple True (Left "size")) . -- (Tuple $ Right (BoolLit True):|[Left "size"]) .
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
        (\c -> Let "ctrl" (ctrlTuple False (Right 1)) . -- (Tuple (Right $ BoolLit False) (Right $ NumericLit 1)) .
               Stmt (SendData $ SSend c $ Left "ctrl"))
        $ Assign "size" $ Increment "size") $
      g collectOut (\c -> Stmt $ SendData $ SSend c $ Left "size") $
      g ctrlOut (\c -> Let "ctrl" (ctrlTuple True (Right 0)) . -- (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) .
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


genInput :: DataIn embExpr annot ty -> TaskExpr embExpr annot ty
genInput (Var bnd)   = L.Var bnd
genInput (Receive r) = ReceiveData r
genInput (Expr t)    = t

fuse :: Binding -> Op embExpr annot ty -> Op embExpr annot ty
fuse var (SMap _ a b c)  = SMap (Var var) a b c
fuse var (Collect _ a b) = Collect (Var var) a b
