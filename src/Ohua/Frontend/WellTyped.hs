{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Ohua.Frontend.WellTyped where

import Ohua.Prelude

import Control.Lens (Traversal')
import Control.Lens.Plated (Plated, gplate, plate)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.List.NonEmpty as NE
import GHC.Exts


data Pat ty
    = VarP Binding (VarType ty)
    | TupP (NonEmpty (Pat ty))
--    | WildP (VarType ty)
    deriving (Show, Eq, Generic)

patType :: Pat ty -> VarType ty
patType = \case
    VarP _ ty -> ty
    TupP ps -> TupleTy (map patType ps)

patBnd :: Pat ty -> NonEmpty Binding
patBnd = \case
    VarP bnd ty -> bnd :| []
    TupP (ps) -> neConcat $ map patBnd ps

patTyBnds :: Pat ty -> NonEmpty (Binding, VarType ty)
patTyBnds = \case
    VarP bnd ty -> (bnd, ty) :| []
    TupP (ps) -> neConcat $ map patTyBnds ps

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
           (NonEmpty (Expr ty))
    | LamE (NonEmpty (Pat ty))
           (Expr ty) -- ^ An expression creating a function
    | IfE (Expr ty)
          (Expr ty)
          (Expr ty)
    | WhileE (Expr ty)
             (Expr ty)
    | MapE (Expr ty) -- ^ Map expression that 'maps' its first argument to its second :: map f xs.
           (Expr ty)
    | BindE (Expr ty)
            (Expr ty) -- ^ @BindE state function@ binds @state@ to be operated on by @function@
    | StmtE (Expr ty)
            (Expr ty) -- ^ An expression with the return value ignored
--    | SeqE (Expr ty)
--           (Expr ty)
    | TupE (NonEmpty (Expr ty)) -- ^ create a tuple value that can be destructured
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
            TupE es -> TupE <$> traverse f es
            AppE e es -> AppE <$> f e <*> traverse f es
            other -> gplate f other

instance IsList (Expr ty) where
    type Item (Expr ty) = Expr ty
    fromList [] =  LitE UnitLit
    fromList (e:es) = TupE (e:| es)


-- ToDo: These are currently used in the Lowering tests but actually without a default type (i.e. after
--       removing 'TypeVar') it doen't make to much sense any more to turn strings into pattern or vars
instance IsString (Expr ty) where
    fromString = (\bnd -> VarE bnd TypeNat). fromString

instance IsString (Pat ty) where
    fromString = (\bnd -> VarP bnd TypeNat). fromString

instance IsList (Pat ty) where
    type Item (Pat ty) = (Pat ty)
    fromList (p:ps) = TupP (p:| ps)
    -- Reminder: Depending on where this is needed we could create a WildP from an empty list.
    fromList [] = error $ "Cannot create a tuple pattern from an empty list"
    toList p = error $ "Ohua tried to convert the pattern "
                <>show p <>"into a list, which is not supported"

