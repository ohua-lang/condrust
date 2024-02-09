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
  , exprType
  , funType
  -- ** Builtin Function Literals
  , ifBuiltin
  , smapBuiltin
--  , seqBuiltin
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

import qualified Ohua.Core.InternalFunctions as IFuns
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Lens.Plated


-------------------- Basic ALang types --------------------
-- ToDo: This needs to include the functions from Alang Refs as constructors
-- | An expression in the algorithm language.
data Expr embExpr ty 
    = Var (TypedBinding ty)  -- ^ Reference to a value via binding: @x@ -> @Var "x"@
    | Lit (Lit embExpr ty Resolved) -- ^ A literal: @2@, @ns/func@ etc -> @Lit (NumericLit 2)@
    | Let (TypedBinding ty) (Expr embExpr ty) (Expr embExpr ty) -- ^ Create and assign a binding: @let bnd = val in expr@ -> @Let "bnd" val expr@
    | Apply (Expr embExpr ty) (Expr embExpr ty) -- ^ Function application: @function arg@ -> @Apply function arg@
    | Lambda (TypedBinding ty) (Expr embExpr ty) -- ^ A lambda function: @\\arg -> body@ -> @Lambda "arg" body@
    | BindState (Expr embExpr ty) (Expr embExpr ty) -- ^ Binding a state value @state#method@ -> @BindState state method@
    deriving (Show, Eq, Generic)

type AExpr = Expr


exprType :: Expr embExpr ty -> OhuaType ty Resolved
exprType e = case e of
    Var (TBind _bnd varTy) -> varTy
    -- We aim for "return types" here so if the literal is a function, this will evaluate to the functions return type
    Lit lit -> getLitType lit
    -- Let should be typed by e2, because this is what it resturns
    Let _bnd _e1 e2 -> exprType e2
    -- Apply (\x:T1. term:T2) (t:T1), should obviously be typed as T2
    -- Obviously we should typecheck AND with generic functions T2 could depend on T1.
    -- However we consider generics in the host language a problem of the host languange i.e.
    -- if the frontend integration allows for generics, than the backend should accept them .. we don't do the derivation
    Apply funE _argE -> returnType funE
    -- Type of an abstraction (\x:T1. term:T2) should be T1 -> T2
    Lambda _argE termE -> exprType termE
    -- Type of a method Bind obj:TO (\x:T1. term:T2) is actually (TO, T2), but we mean the type it should
    -- explicitely evaluate to in the host language so it is T2
    BindState _objE methodcallE -> exprType methodcallE

returnType :: Expr embExpr ty -> OhuaType ty Resolved
returnType e = case funType e of
    Just (FunType _ out) -> out
    Just (STFunType _ _ out) -> out
    Nothing -> exprType e

-- ToDo: This is actually very _unelegant_ but in some instances we need the function type from
--       Lambda Expression, while in others we only care about the type the term will evaluate to.
--       I'm currently not sure, what the best way to handle this would be?!

funType :: Expr embExpr ty -> Maybe (FunType ty Resolved)
funType e = case e of
        (Var (TBind _bnd (FType fTy)))          -> Just fTy
        (Lit (FunRefLit fRef))                  -> Just $ getRefType fRef
        (Lambda tBnd term)                      -> Just $ FunType (Right $ asType tBnd :| []) (exprType term)
        (BindState _state method)               -> funType method
        -- Question: What's the type of BindState at this point?
        _other                                   -> Nothing



-------------------- Convenience functions ------------------------------

pureFunction :: QualifiedBinding -> FunType ty Resolved-> Expr embExpr ty
pureFunction bnd ty = Lit (FunRefLit (FunRef bnd ty))

mkFunLit :: QualifiedBinding -> FunType ty Resolved-> Expr embExpr ty
mkFunLit qBnd = pureFunction qBnd 



-- Specific Functions, that should become part of the language ---------


ifBuiltin :: OhuaType ty Resolved -> Expr embExpr ty
ifBuiltin vTy = mkFunLit IFuns.ifThenElse (FunType (Right $ IType TypeBool :| [vTy ,vTy]) vTy)

{-
seqBuiltin :: OhuaType ty Resolved ->  OhuaType ty Resolved ->  Expr embExpr ty
-- Operator to sequence statements igrnoring the result of the first one
-- ToDo: check if we still need that anywhere
seqBuiltin fstTy scndTy =  mkFunLit IFuns.seq (FunType [fstTy, scndTy] scndTy)
--}

smapBuiltin :: OhuaType ty Resolved ->  OhuaType ty Resolved -> OhuaType ty Resolved -> Expr embExpr ty
-- HO for-loop: function -> collection to apply function to -> typeOfCollection ( return Type function)
smapBuiltin fnTy collTy resTy = mkFunLit IFuns.smap (FunType (Right $ fnTy :| [collTy]) resTy )


-------------------- Recursion schemes support -------------------------

makeBaseFunctor ''Expr

instance Container (ExprF embExpr ty a)

-------------------- Convenience patterns ------------------------------

pattern PureFunction :: QualifiedBinding -> Expr embExpr ty
pattern PureFunction bnd  <- Lit (FunRefLit (FunRef bnd  _))

pattern PureFunctionTy :: QualifiedBinding  -> FunType ty Resolved-> Expr embExpr ty
pattern PureFunctionTy bnd  ty <- Lit (FunRefLit (FunRef bnd  ty))

pattern PureFunctionF :: QualifiedBinding  -> ExprF embExpr ty a
pattern PureFunctionF bnd  <- LitF (FunRefLit (FunRef bnd  _))

pattern StatefulFunction :: QualifiedBinding  -> Expr embExpr ty -> Expr embExpr ty
pattern StatefulFunction bnd  expr <- BindState expr (Lit (FunRefLit (FunRef bnd  _)))

pattern StatefulFunctionTy :: QualifiedBinding  -> FunType ty Resolved-> Expr embExpr ty -> Expr embExpr ty
pattern StatefulFunctionTy bnd  ty expr <- BindState expr (Lit (FunRefLit (FunRef bnd  ty)))

pattern StatefulFunctionF :: QualifiedBinding -> Expr embExpr ty -> ExprF embExpr ty (Expr embExpr ty)
pattern StatefulFunctionF bnd  expr <- BindStateF expr (Lit (FunRefLit (FunRef bnd  _)))


-------------------- Additional type class instances --------------------

{-Actually AEprx isn't a string any more, it's at least a string and a type
instance IsString (AExpr embExpr ty) where
    fromString = fromString >>> \case
        Unqual bnd -> Var bnd
        Qual q -> pureFunction q Nothing Untyped
-}

instance Plated (Expr embExpr ty) where plate = gplate

-- instance Embed (Expr embExpr ty) Int where
--     embedE = embedE . fromIntegral @Int @Integer
-- instance Embed (Expr embExpr ty) Integer where
--     embedE = embedE . NumericLit 
instance Embed (Expr embExpr ty) (Lit embExpr ty Resolved) where
    embedE = Lit
instance Embed (Expr embExpr ty) (TypedBinding ty) where
    embedE = Var
instance Embed (Expr embExpr ty) (FunRef ty Resolved) where
    embedE frLit = Lit (FunRefLit frLit)
-- instance Embed (Expr embExpr ty) QualifiedBinding where
--     embedE = embedE . (\qb -> FunRef qb Untyped)

-------------------- Additional Traversals --------------------

-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM ::
       Monad m
    => (Expr embExpr ty -> m (Expr embExpr ty))
    -> Expr embExpr ty
    -> m (Expr embExpr ty)
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
    => (Expr embExpr ty -> m (Expr embExpr ty))
    -> Expr embExpr ty
    -> m (Expr embExpr ty)
lrPostwalkExprM f e =
    f =<<
    case e of
        Let assign val body ->
            Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
        Apply fn arg -> Apply <$> lrPostwalkExprM f fn <*> lrPostwalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
        _ -> return e

-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr :: (Expr embExpr ty -> Expr embExpr ty) -> Expr embExpr ty -> Expr embExpr ty
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)


