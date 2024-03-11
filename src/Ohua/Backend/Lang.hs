{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Ohua.Backend.Lang where

import Ohua.Commons.Prelude hiding (First)

import qualified Text.Show

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful expr QualifiedBinding [expr]
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (App expr)

data ComType = Channel | Recv | Send deriving (Show, Eq, Generic)

data Com (f::ComType) (embExpr::Type) (annot:: Type) (ty::Type) :: Type where
  SChan :: Binding -> Com 'Channel embExpr annot ty
  SRecv :: OhuaType ty Resolved -> Com 'Channel embExpr annot ty -> Com 'Recv embExpr annot ty
  SSend :: Com 'Channel embExpr annot ty -> Either Binding (Lit embExpr ty Resolved) -> Com 'Send embExpr annot ty

instance Eq (Com semTy embExpr annot ty) where
  SChan bnd0 == SChan bnd1 = bnd0 == bnd1
  SRecv _ chan0 == SRecv _ chan1 = chan0 == chan1
  SSend chan0 bnd0 == SSend chan1 bnd1 = chan0 == chan1 && bnd0 == bnd1

instance Show (Com semTy embExpr annot ty) where
  show (SChan bnd) = "Chan: " <> show bnd
  show (SRecv ty chan) = "Recv: <" <> show ty <> "> " <>show chan
  show (SSend chan bnd) = "Send: " <> show chan <> " bnd:" <> show bnd

type Channel = Com 'Recv

instance Hashable (Com semTy embExpr annot ty) where
  hashWithSalt s (SChan bnd) = s `hashWithSalt` bnd
  hashWithSalt s (SRecv _ chan) = s `hashWithSalt` chan
  hashWithSalt s (SSend chan bnd) = s `hashWithSalt` chan `hashWithSalt` bnd

data List expr = Create | Append Binding expr
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (List expr)

data TaskExpr embExpr annot ty
  = Var Binding
  | Lit (Lit embExpr ty Resolved) -- true, false  etc.
  | Apply (App (TaskExpr embExpr annot ty))
  | Let Binding
        (TaskExpr embExpr annot ty)
        (TaskExpr embExpr annot ty) -- cont
  | Stmt
      (TaskExpr embExpr annot ty)
      (TaskExpr embExpr annot ty) -- cont
  | Assign -- side-effect
      Binding
      (TaskExpr embExpr annot ty)
  | ReceiveData (Com 'Recv embExpr annot ty)
  | SendData (Com 'Send embExpr annot ty)

  -- specific control flow:
  | EndlessLoop (TaskExpr embExpr annot ty)
  | ForEach Binding Binding (TaskExpr embExpr annot ty) -- ^ a.k.a. map
  | Repeat (Either Binding Int) (TaskExpr embExpr annot ty)
  | While (TaskExpr embExpr annot ty) (TaskExpr embExpr annot ty)
  | Cond (TaskExpr embExpr annot ty) (TaskExpr embExpr annot ty) (TaskExpr embExpr annot ty)

  -- specific functions:
  | HasSize Binding -- :: [a] -> Bool
  | Size Binding -- :: [a] -> Int

  | ListOp (List (TaskExpr embExpr annot ty))

  | Tuple ( NonEmpty (Either Binding (Lit embExpr ty Resolved)))
  | Indexing Binding Integer

  | Increment Binding -- a + 1;
  | Decrement Binding -- a - 1;
  | Not (TaskExpr embExpr annot ty)

  deriving (Show,Eq,Generic)

instance Hashable (TaskExpr embExpr annot ty)

-- To ease adaption of code sites using the former First and Second
-- TODO: Obviously doing it this way we loose the 'type-control' of indexing
-- maybe we can restore that later using Singletons? 
firstIndexing:: Binding -> TaskExpr embExpr annot ty
firstIndexing bnd = Indexing bnd 0

secondIndexing:: Binding -> TaskExpr embExpr annot ty
secondIndexing bnd = Indexing bnd 1


-- FIXME remove this code ASAP
containsBinding :: TaskExpr embExpr annot ty -> Binding -> Bool
containsBinding (Var bnd ) b = b == bnd
containsBinding Lit{} _ = False
containsBinding (Apply app) b = case app of
  (Stateless _ exprs) -> any (`containsBinding` b) exprs
  (Stateful e _ exprs) -> containsBinding e b || any (`containsBinding` b) exprs
containsBinding (Let bnd expr cont) b = bnd == b || containsBinding expr b || containsBinding cont b
containsBinding (Stmt expr cont) b = containsBinding expr b || containsBinding cont b
containsBinding (Assign bnd effect) b = bnd == b || containsBinding effect b
containsBinding ReceiveData{} _ = False
containsBinding (SendData (SSend _ (Left bnd))) b = bnd == b
containsBinding (SendData (SSend _ (Right (EnvRefLit bnd _ty)))) b = bnd == b
containsBinding (SendData (SSend _ _)) _ = False
containsBinding (EndlessLoop expr) b = containsBinding expr b
containsBinding (ForEach bnd1 bnd2 expr) b = bnd1 == b || bnd2 == b || containsBinding expr b
containsBinding (Repeat cond expr) b = either (== b) (const False) cond || containsBinding expr b
containsBinding (While cond expr) b = containsBinding cond b || containsBinding expr b
containsBinding (Cond cond yesBranch noBranch) b = containsBinding cond b || containsBinding yesBranch b || containsBinding noBranch b
containsBinding (HasSize bnd) b = bnd == b
containsBinding (Size bnd) b = bnd == b
containsBinding (ListOp Create) _ = False
containsBinding (ListOp (Append bnd expr)) b = bnd == b || containsBinding expr b
containsBinding (Tuple bndsOrLits) b = foldl' (\found v -> found || containsB v b) False bndsOrLits
containsBinding (Indexing bnd _idx) b = bnd == b
containsBinding (Increment bnd) b = bnd == b
containsBinding (Decrement bnd) b = bnd == b
containsBinding (Not expr) b = containsBinding expr b

containsB:: Either Binding (Lit embExpr ty Resolved)-> Binding -> Bool
containsB v b = case v of
  Left bnd -> bnd == b
  Right _ -> False



-------------------- Recursion schemes support --------------------

makeBaseFunctor ''App
makeBaseFunctor ''TaskExpr

instance Plated (TaskExpr embExpr annot ty) where plate = gplate
