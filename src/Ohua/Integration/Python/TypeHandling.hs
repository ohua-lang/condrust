{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeHandling where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import qualified Language.Python.Common as Py



import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.List.NonEmpty


data PythonVarType = PythonObject deriving (Show, Eq)

defaultName :: Binding
defaultName = "PythonObject"

instance Pathable PythonVarType where
    toPath _t = Just (Left defaultName) 

instance Pretty PythonVarType where
    pretty PythonObject = "obj"

type FunTypesMap = HM.HashMap QualifiedBinding (FunType PythonVarType Resolved)

data Module = Module FilePath (Py.Module Py.SrcSpan)