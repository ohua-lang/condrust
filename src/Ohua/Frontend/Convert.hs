module Ohua.Frontend.Convert where

import Ohua.Prelude

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang

class ConvertFrom a where
    convertFrom :: CompM m => a -> m FrLang.Expr