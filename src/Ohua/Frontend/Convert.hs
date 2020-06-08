module Ohua.Frontend.Convert where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang

class ConvertExpr a where
    convertExpr :: CompM m => a -> m FrLang.Expr

class ConvertPat a where
    convertPat :: CompM m => a -> m FrLang.Pat