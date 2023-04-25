{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeExtraction where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST 
import Language.Python.Common.SrcLocation


import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty


data PythonArgType = PythonObject
type PythonTypeAnno  = PythonArgType  
type FunTypes = HM.HashMap QualifiedBinding (FunType PythonTypeAnno)

