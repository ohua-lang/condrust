{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds, DeriveGeneric #-}
module Ohua.Backend.Operators.Common where

import Ohua.Prelude

import Ohua.Backend.Lang

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS


newtype OutputChannel ty = OutputChannel (Com 'Channel ty) deriving (Eq, Show, Generic)
instance Hashable (OutputChannel ty)
instance Ord (OutputChannel ty) where
    compare = compare `on` unwrapBnd

-- FIXME Should use annotations ST and Data from BindingType
data VarReceive ty
    = StateVar (OutputChannel ty) (Com 'Recv ty)
    | PureVar (OutputChannel ty) (Com 'Recv ty)
    deriving (Eq,Generic)

instance Hashable (VarReceive ty)

fromVarReceive :: VarReceive ty -> (OutputChannel ty, Com 'Recv ty)
fromVarReceive (StateVar c r) = (c,r)
fromVarReceive (PureVar c r) = (c,r)

unwrapChan :: OutputChannel ty -> Com 'Channel ty
unwrapChan (OutputChannel c) = c

unwrapBnd :: OutputChannel ty -> Binding 
unwrapBnd (OutputChannel (SChan b)) = b

initVarsExpr :: [(Binding, Com 'Recv ty)] -> [OutputChannel ty] -> TaskExpr ty -> TaskExpr ty
initVarsExpr rVars sVars cont = 
    foldr (\(bnd, e) c -> Let bnd e c) cont $ toExpr rVars sVars

receiveVarsExpr :: [(OutputChannel ty, Com 'Recv ty)] -> [OutputChannel ty] -> TaskExpr ty
receiveVarsExpr rVars sVars = 
    foldr (\(OutputChannel (SChan bnd), e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: [(a, Com 'Recv ty)] -> [OutputChannel ty] -> [(a, TaskExpr ty)]
toExpr rVars sVars  = map (second f) rVars
    where
        f (SRecv _ ch@(SChan bnd)) | HS.member ch ctrled = Var bnd
        f r  = ReceiveData r
        ctrled = HS.fromList $ map unwrapChan sVars
