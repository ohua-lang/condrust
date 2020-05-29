module Ohua.Backend.Convert where

import Ohua.Prelude

import Ohua.Backend.Lang

class ConvertInto a where
    convertExpr :: TCExpr -> a