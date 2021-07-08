module Ohua.Integration.Python.Types where

import Ohua.Prelude
import qualified Language.Python.Common as PyCom

data Module = Module FilePath (PyCom.Module PyCom.SrcSpan)
