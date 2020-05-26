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

data Expr
    = VarE Binding
    | LitE Lit
    | LetE Pat
           Expr
           Expr
    | AppE Expr
           [Expr]
    | LamE [Pat]
           Expr -- ^ An expression creating a function
    | IfE Expr
          Expr
          Expr
    | MapE Expr
           Expr
    | BindE Expr
            Expr -- ^ @BindE state function@ binds @state@ to be operated on by @function@
    | StmtE Expr
            Expr -- ^ An expression with the return value ignored
    | SeqE Expr
           Expr
    | TupE [Expr] -- ^ create a tuple value that can be destructured
    deriving (Show, Eq, Generic)

patterns :: Traversal' Expr Pat
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

instance NFData Pat

makeBaseFunctor ''Expr

instance Plated Expr where
    plate f =
        \case
            TupE es -> TupE <$> traverse f es
            AppE e es -> AppE <$> f e <*> traverse f es
            other -> gplate f other

instance Hashable Expr

instance NFData Expr

instance IsString Expr where
    fromString = VarE . fromString

instance IsList Expr where
    type Item Expr = Expr
    fromList = TupE

instance IsString Pat where
    fromString = VarP . fromString

instance IsList Pat where
    type Item Pat = Pat
    fromList = TupP

