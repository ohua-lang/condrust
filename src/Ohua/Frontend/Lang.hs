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

data Pat
    = VarP Binding
    | TupP [Pat]
    | UnitP
    deriving (Show, Eq, Generic)

data Expr ty
    = VarE Binding
    | LitE (Lit ty)
    | LetE Pat
           (Expr ty)
           (Expr ty)
    | AppE (Expr ty)
           [Expr ty]
    | LamE [Pat]
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
    | TupE [Expr ty] -- ^ create a tuple value that can be destructured
    deriving (Show, Generic)

patterns :: Traversal' (Expr ty) Pat
patterns f =
    \case
        LamE ps e -> flip LamE e <$> traverse f ps
        LetE p e1 e2 -> (\p' -> LetE p' e1 e2) <$> f p
        o -> pure o

makeBaseFunctor ''Pat

instance Plated Pat where
    plate f =
        \case
            TupP ps -> TupP <$> traverse f ps
            other -> gplate f other

instance Hashable Pat

makeBaseFunctor ''Expr

instance Plated (Expr ty) where
    plate f =
        \case
            TupE es -> TupE <$> traverse f es
            AppE e es -> AppE <$> f e <*> traverse f es
            other -> gplate f other

instance IsString (Expr ty) where
    fromString = VarE . fromString

instance IsList (Expr ty) where
    type Item (Expr ty) = Expr ty
    fromList = TupE

instance IsString Pat where
    fromString = VarP . fromString

instance IsList Pat where
    type Item Pat = Pat
    fromList = TupP

