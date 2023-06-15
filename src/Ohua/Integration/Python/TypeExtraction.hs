{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeExtraction where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST 
import Language.Python.Common.SrcLocation


import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.List.NonEmpty


data PythonVarType = PythonObject deriving (Show, Eq)

instance Pretty PythonVarType where
    pretty PythonObject = "obj"

type FunTypes = HM.HashMap QualifiedBinding (FunType PythonVarType)