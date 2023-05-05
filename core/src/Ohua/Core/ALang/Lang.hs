-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- This module defines the algorithm language. An intermediate language based on
-- the call-by-need lambda calculus.
--
-- The basic building block is the 'Expr' type.
--
-- == Traversals
--
-- ALang leverages the power of __two__ /scrap-your-boilerplate/ libraries,
-- @recursion-schemes@ and @uniplate@. Both libraries define generic ways of
-- traversing the entire expression tree easily. Depending on the function used
-- various kinds of information can be propagated up or down the tree.
--
-- Generally speaking the interface for the @uniplate@ library is easier to
-- understand for beginners, whereas the @recursion-schemes@ library is, in my
-- opinion, more type safe.
--
-- For anyone interested in learning about recursion schemes (which is also
-- applicable to @uniplate@) I recommend
-- <https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/ this multi part blog post>
-- by Patrick Thomson.
--
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Core.ALang.Lang
  ( Expr(..)
  , AExpr
  , TypedBinding(..), asBnd, asType
  , exprType
  -- ** Convenience patterns
  , pattern PureFunction, pattern PureFunctionTy, pattern PureFunctionF
  , pattern StatefulFunction, pattern StatefulFunctionTy, pattern StatefulFunctionF
 -- ** Conveniencefunctions
  , pureFunction
 -- ** The recursion schemes base functor
  , ExprF(..)
  -- ** Additional Traversals
  , lrPostwalkExpr
  , lrPostwalkExprM, lrPrewalkExprM
  ) where

import Ohua.Prelude

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Lens.Plated
-- import Language.Haskell.TH.Syntax (Lift)
import Control.Category ((>>>))

import Ohua.Core.Types

-------------------- Basic ALang types --------------------

-- | An expression in the algorithm language.
data Expr ty
    = Var (TypedBinding ty)  -- ^ Reference to a value via binding: @x@ -> @Var "x"@
    | Lit (Lit ty) -- ^ A literal: @2@, @ns/func@ etc -> @Lit (NumericLit 2)@
    | Let (TypedBinding ty) (Expr ty) (Expr ty) -- ^ Create and assign a binding: @let bnd = val in expr@ -> @Let "bnd" val expr@
    | Apply (Expr ty) (Expr ty) -- ^ Function application: @function arg@ -> @Apply function arg@
    | Lambda (TypedBinding ty) (Expr ty) -- ^ A lambda function: @\\arg -> body@ -> @Lambda "arg" body@
    | BindState (Expr ty) (Expr ty) -- ^ Binding a state value @state#method@ -> @BindState state method@
    deriving (Show, Eq, Generic)

type AExpr = Expr
-- ToDo: Tyed Binding realy does belong in into commons as we need it everywhere in DFLang and
--       could already start using it in FRLang I think
data TypedBinding ty = TBind Binding (ArgType ty) deriving (Show, Eq, Generic)

asBnd :: TypedBinding ty -> Binding
asBnd (TBind bnd _ty) = bnd 

asType :: TypedBinding ty -> ArgType ty
asType (TBind _bnd ty) = ty 

-- ToDo: Actually every expression should be typable at this point
exprType:: Expr ty -> ArgType ty
exprType e = case e of 
    Var (TBind bnd ty) -> ty 
    Lit lit -> case getArgType lit of
        Just ty -> ty
        Nothing -> TypeVar 
    -- Let should be typed by e2, because this is what it resturns
    Let bnd e1 e2 -> error $ "Trying to retrieve the type of a `let expr`, which are currently not typed. Please report this error" 
    -- Apply (\x:T1. term:T2) (t:T1), should obviously be typed as T2
    Apply funE argE -> error $ "Trying to retrieve the type of a `apply expr`, which are currently not typed. Please report this error" 
    -- Type of an abstraction (\x:T1. term:T2) should be T1 -> T2
    Lambda argE termE -> error $ "Trying to retrieve the type of a `lambda expr`, which are currently not typed. Please report this error" 
    -- Type of a method Bind obj:TO (\x:T1. term:T2) is (TO, T2), because a new state and the return term are
    -- returned
    BindState objE methodcallE -> error $ "Trying to retrieve the type of a `method call expr`, which are currently not typed. Please report this error" 


instance Ord (TypedBinding ty)
instance Hashable (TypedBinding ty)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Expr

-- deriving instance Lift a => Lift (ExprF ty a)

-- deriving instance Eq a => Eq (ExprF ty a)
--deriving instance (Ord bndType, Ord refType, Ord a) => Ord (AExprF bndType refType a)

instance Container (ExprF ty a)

-------------------- Convenience patterns --------------------

pattern PureFunction :: QualifiedBinding -> Maybe FnId -> Expr ty
pattern PureFunction bnd ident <- Lit (FunRefLit (FunRef bnd ident _))

pattern PureFunctionTy :: QualifiedBinding -> Maybe FnId -> FunType ty -> Expr ty
pattern PureFunctionTy bnd ident ty <- Lit (FunRefLit (FunRef bnd ident ty))

pattern PureFunctionF :: QualifiedBinding -> Maybe FnId -> ExprF ty a
pattern PureFunctionF bnd ident <- LitF (FunRefLit (FunRef bnd ident _))

pattern StatefulFunction :: QualifiedBinding -> Maybe FnId -> Expr ty -> Expr ty
pattern StatefulFunction bnd ident expr <- BindState expr (Lit (FunRefLit (FunRef bnd ident _)))

pattern StatefulFunctionTy :: QualifiedBinding -> Maybe FnId ->FunType ty -> Expr ty -> Expr ty
pattern StatefulFunctionTy bnd ident ty expr <- BindState expr (Lit (FunRefLit (FunRef bnd ident ty)))

pattern StatefulFunctionF :: QualifiedBinding -> Maybe FnId -> Expr ty -> ExprF ty (Expr ty)
pattern StatefulFunctionF bnd ident expr <- BindStateF expr (Lit (FunRefLit (FunRef bnd ident _)))


-------------------- Convenience functions --------------------

pureFunction :: QualifiedBinding -> Maybe FnId -> FunType ty -> Expr ty
pureFunction bnd ident ty = Lit (FunRefLit (FunRef bnd ident ty))


-------------------- Additional type class instances --------------------

{-Actually AEprx isn't a string any more, it's at least a string and a type
instance IsString (AExpr ty) where
    fromString = fromString >>> \case
        Unqual bnd -> Var bnd
        Qual q -> pureFunction q Nothing Untyped
-}

instance Plated (Expr ty) where plate = gplate

-- instance Embed (Expr ty) Int where
--     embedE = embedE . fromIntegral @Int @Integer
-- instance Embed (Expr ty) Integer where
--     embedE = embedE . NumericLit 
instance Embed (Expr ty) (Lit ty) where
    embedE = Lit
instance Embed (Expr ty) (TypedBinding ty) where
    embedE = Var
instance Embed (Expr ty) (FunRef ty) where
    embedE = embedE . FunRefLit
-- instance Embed (Expr ty) QualifiedBinding where
--     embedE = embedE . (\qb -> FunRef qb Nothing Untyped)

-------------------- Additional Traversals --------------------

-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM ::
       Monad m
    => (Expr ty -> m (Expr ty))
    -> Expr ty
    -> m (Expr ty)
lrPrewalkExprM f e =
    f e >>= \case
        Let bnd val body ->
            Let bnd <$> lrPrewalkExprM f val <*> lrPrewalkExprM f body
        Apply fn arg -> Apply <$> lrPrewalkExprM f fn <*> lrPrewalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPrewalkExprM f body
        e' -> return e'

-- | Traverse an ALang expression from left to right and from the bottom up.
lrPostwalkExprM ::
       Monad m
    => (Expr ty -> m (Expr ty))
    -> Expr ty
    -> m (Expr ty)
lrPostwalkExprM f e =
    f =<<
    case e of
        Let assign val body ->
            Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
        Apply fn arg -> Apply <$> lrPostwalkExprM f fn <*> lrPostwalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
        _ -> return e

-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr :: (Expr ty -> Expr ty) -> Expr ty -> Expr ty
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)
