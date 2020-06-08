module Ohua.Backend.Types where

import Ohua.Prelude

import Ohua.Backend.Lang
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as L

class Integration lang where
    backend :: CompM m => Namespace TCExpr -> lang -> m (NonEmpty (FilePath, L.ByteString))
