module Ohua.Integration.Python.Types where

import Ohua.Prelude
import qualified Language.Python.Common as Py

data Module = Module FilePath (Py.Module Py.SrcSpan)
