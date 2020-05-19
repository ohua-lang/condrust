module Ohua.Backend.Convert where

import Ohua.Prelude

import Ohua.Backend.TCLang

class ConvertInto a where
    convertExpr :: TCExpr -> a