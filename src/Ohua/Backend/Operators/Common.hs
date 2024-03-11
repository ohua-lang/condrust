{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds, DeriveGeneric #-}
module Ohua.Backend.Operators.Common where

import Ohua.Commons.Prelude

import Ohua.Backend.Lang

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS


newtype OutputChannel embExpr annot ty = OutputChannel (Com 'Channel embExpr annot ty) deriving (Eq, Show, Generic)
instance Hashable (OutputChannel embExpr annot ty)
instance Ord (OutputChannel embExpr annot ty) where
    compare = compare `on` unwrapBnd

-- FIXME Should use annotations ST and Data from BindingType
data VarReceive embExpr annot ty
    = StateVar (OutputChannel embExpr annot ty) (Com 'Recv embExpr annot ty)
    | PureVar (OutputChannel embExpr annot ty) (Com 'Recv embExpr annot ty)
    deriving (Eq,Generic)

instance Hashable (VarReceive embExpr annot ty)

fromVarReceive :: VarReceive embExpr annot ty -> (OutputChannel embExpr annot ty, Com 'Recv embExpr annot ty)
fromVarReceive (StateVar c r) = (c,r)
fromVarReceive (PureVar c r) = (c,r)

unwrapChan :: OutputChannel embExpr annot ty -> Com 'Channel embExpr annot ty
unwrapChan (OutputChannel c) = c

unwrapBnd :: OutputChannel embExpr annot ty -> Binding
unwrapBnd (OutputChannel (SChan b)) = b

initVarsExpr :: [(Binding, Com 'Recv embExpr annot ty)] -> [OutputChannel embExpr annot ty] -> TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty
initVarsExpr rVars sVars cont =
    foldr (\(bnd, e) c -> Let bnd e c) cont $ toExpr rVars sVars

receiveVarsExpr :: [(OutputChannel embExpr annot ty, Com 'Recv embExpr annot ty)] -> [OutputChannel embExpr annot ty] -> TaskExpr embExpr annot ty
receiveVarsExpr rVars sVars =
    foldr (\(OutputChannel (SChan bnd), e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: [(a, Com 'Recv embExpr annot ty)] -> [OutputChannel embExpr annot ty] -> [(a, TaskExpr embExpr annot ty)]
toExpr rVars sVars  = map (second f) rVars
    where
        f (SRecv _ ch@(SChan bnd)) | HS.member ch ctrled = Var bnd
        f r  = ReceiveData r
        ctrled = HS.fromList $ map unwrapChan sVars

ctrlTuple :: Bool -> Either Binding Integer -> TaskExpr embExpr annot ty
ctrlTuple bl eth =
  let second = case eth of
        Left bind -> Left bind
        Right n -> Right (NumericLit n)
  in (Tuple $ Right (BoolLit bl):|[second])
