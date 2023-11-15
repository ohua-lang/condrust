{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeHandling where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import qualified Language.Python.Common as Py



import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.List.NonEmpty as NE

data PythonObject = PythonObject deriving (Show, Eq) 
data PythonVarType = PSingle PythonObject | PTuple (NonEmpty PythonObject) deriving (Show, Eq)

defaultName :: Binding
defaultName = "PythonObject"

instance Pathable PythonVarType where
    toPath (PSingle t) = Just (Left defaultName)
    toPath (PTuple ts) = Nothing 

instance Pretty PythonVarType where
    pretty (PSingle t) = "obj"
    pretty (PTuple ts) = "("<> foldr (\o s -> "obj, " <> s) "" ts <>")"

-- Most builtin Python types can be interpreted as truth values in a way
-- FIXME: If requried in any way, make this more specific (will require changes to the PythonVarType)
instance TruthableType PythonVarType where 
    canbeBool _pt = True

instance UnTuple PythonVarType where
    unTupleType (PTuple ts) = NE.map PSingle ts
    unTupleType t = t:|[]
     
type FunTypesMap = HM.HashMap QualifiedBinding (FunType PythonVarType Resolved)

data Module = Module FilePath (Py.Module Py.SrcSpan)