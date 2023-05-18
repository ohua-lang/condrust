{-# LANGUAGE TemplateHaskell #-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
    , PatF(..)
    , ExprF(..)
    , patterns
    ) where

import Ohua.Prelude

import Control.Lens (Traversal')
import Control.Lens.Plated (Plated, gplate, plate)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import GHC.Exts

data Pat ty
    = VarP Binding (VarType ty)
    -- Actualy a tuple pattern starts making sense at two or more patterns
    -- but it should have at least one. So I changed this from [] to NonEmpty
    | TupP (NonEmpty (Pat ty))
    | UnitP
    deriving (Show, Eq, Generic)

data Expr ty
    -- REMINDER: We need to wrap the host type in an VarType here, because
    -- the compiler will introdude variables typed as internal bool/unit/int 
    -- ... that also have to be representable
    = VarE Binding (VarType ty)
    | LitE (Lit ty)
    | LetE (Pat ty)
           (Expr ty)
           (Expr ty)
    | AppE (Expr ty)
           [Expr ty]
    | LamE [Pat ty]
           (Expr ty) -- ^ An expression creating a function
    | IfE (Expr ty)
          (Expr ty)
          (Expr ty)
    | WhileE (Expr ty) (Expr ty)
    | MapE (Expr ty)
           (Expr ty)
    | BindE (Expr ty)
            (Expr ty) -- ^ @BindE state function@ binds @state@ to be operated on by @function@
    | StmtE (Expr ty)
            (Expr ty) -- ^ An expression with the return value ignored
    | SeqE (Expr ty)
           (Expr ty)
    | TupE (FunType ty) [Expr ty] -- ^ create a tuple value that can be destructured
    deriving (Show, Generic)

patterns :: Traversal' (Expr ty) (Pat ty)
patterns f =
    \case
        LamE ps e -> flip LamE e <$> traverse f ps
        LetE p e1 e2 -> (\p' -> LetE p' e1 e2) <$> f p
        o -> pure o

makeBaseFunctor ''Pat

instance Plated (Pat ty) where
    plate f =
        \case
            TupP ps -> TupP <$> traverse f ps
            other -> gplate f other

instance Hashable (Pat ty)

makeBaseFunctor ''Expr

instance Plated (Expr ty) where
    plate f =
        \case
            TupE ty es -> TupE ty <$> traverse f es
            AppE e es -> AppE <$> f e <*> traverse f es
            other -> gplate f other

instance IsString (Expr ty) where
    fromString = (\bnd -> VarE bnd TypeVar). fromString

instance IsList (Expr ty) where
    type Item (Expr ty) = Expr ty
    fromList [] = TupE (FunType [] TypeVar ) []
    fromList a@(_:xs) = TupE (FunType (TypeVar : (map (const TypeVar) xs)) TypeVar) a

instance IsString (Pat ty) where
    fromString = (\bnd -> VarP bnd TypeVar). fromString

instance IsList (Pat ty) where
    type Item (Pat ty) = (Pat ty)
    fromList (p:ps) = TupP (p:| ps)
    -- Reminder: Depending on where this is needed we could create a UnitP from an empty list.
    fromList [] = error $ "Cannot create a tuple pattern from an empty list"
    toList p = error $ "Ohua tried to convert the pattern "
                <>show p <>"into a list, which is not supported"

