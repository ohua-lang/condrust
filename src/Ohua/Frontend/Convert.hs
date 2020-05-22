module Ohua.Frontend.Convert where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang

class ConvertFrom a where
    convertFrom :: a -> FrLang.Expr