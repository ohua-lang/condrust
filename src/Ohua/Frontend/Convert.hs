module Ohua.Frontend.Convert where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang

class ConvertExpr a where
    convertExpr :: ErrAndLogM m => a -> m (FrLang.Expr ty)

class ConvertPat a where
    convertPat :: ErrAndLogM m => a -> m (FrLang.Pat ty)