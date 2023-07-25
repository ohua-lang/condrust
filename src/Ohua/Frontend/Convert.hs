module Ohua.Frontend.Convert where

import Ohua.Prelude

import Ohua.Frontend.Lang

class ConvertExpr a where
    convertExpr :: ErrAndLogM m => a -> m (UnresolvedExpr ty)

class ConvertPat a where
    convertPat :: ErrAndLogM m => a -> m (UnresolvedPat ty)
