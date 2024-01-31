{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds, DeriveGeneric #-}
module Ohua.Backend.Operators.Common where

import Ohua.Prelude

import Ohua.Backend.Lang

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS


newtype OutputChannel embExpr ty = OutputChannel (Com 'Channel embExpr ty) deriving (Eq, Show, Generic)
instance Hashable (OutputChannel embExpr ty)
instance Ord (OutputChannel embExpr ty) where
    compare = compare `on` unwrapBnd

-- FIXME Should use annotations ST and Data from BindingType
data VarReceive embExpr ty
    = StateVar (OutputChannel embExpr ty) (Com 'Recv embExpr ty)
    | PureVar (OutputChannel embExpr ty) (Com 'Recv embExpr ty)
    deriving (Eq,Generic)

instance Hashable (VarReceive embExpr ty)

fromVarReceive :: VarReceive embExpr ty -> (OutputChannel embExpr ty, Com 'Recv embExpr ty)
fromVarReceive (StateVar c r) = (c,r)
fromVarReceive (PureVar c r) = (c,r)

unwrapChan :: OutputChannel embExpr ty -> Com 'Channel embExpr ty
unwrapChan (OutputChannel c) = c

unwrapBnd :: OutputChannel embExpr ty -> Binding
unwrapBnd (OutputChannel (SChan b)) = b

initVarsExpr :: [(Binding, Com 'Recv embExpr ty)] -> [OutputChannel embExpr ty] -> TaskExpr embExpr ty -> TaskExpr embExpr ty
initVarsExpr rVars sVars cont =
    foldr (\(bnd, e) c -> Let bnd e c) cont $ toExpr rVars sVars

receiveVarsExpr :: [(OutputChannel embExpr ty, Com 'Recv embExpr ty)] -> [OutputChannel embExpr ty] -> TaskExpr embExpr ty
receiveVarsExpr rVars sVars =
    foldr (\(OutputChannel (SChan bnd), e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: [(a, Com 'Recv embExpr ty)] -> [OutputChannel embExpr ty] -> [(a, TaskExpr embExpr ty)]
toExpr rVars sVars  = map (second f) rVars
    where
        f (SRecv _ ch@(SChan bnd)) | HS.member ch ctrled = Var bnd
        f r  = ReceiveData r
        ctrled = HS.fromList $ map unwrapChan sVars

ctrlTuple :: Bool -> Either Binding Integer -> TaskExpr embExpr ty
ctrlTuple bl eth =
  let second = case eth of
        Left bind -> Left bind
        Right n -> Right (NumericLit n)
  in (Tuple $ Right (BoolLit bl):|[second])
