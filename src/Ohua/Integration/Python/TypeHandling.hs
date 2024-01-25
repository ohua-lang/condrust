{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Ohua.Integration.Python.TypeHandling
Description : Module contains anything related to Python "typing".

The functions references defined in this module are used to 'desugar' Python syntax to function
calls in the frontend and 'resugar' (:-)) them in the backend again 
-}

module Ohua.Integration.Python.TypeHandling where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import qualified Language.Python.Common as Py



import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc (Pretty(..))
import qualified Data.List.NonEmpty as NE

data PythonObject = PythonObject deriving (Show, Eq) 
data PythonVarType = PSingle PythonObject | PTuple (NonEmpty PythonObject) deriving (Show, Eq)

defaultName :: Binding
defaultName = "PythonObject"

defaultType:: OhuaType PythonVarType res
defaultType = HType (HostType (PSingle PythonObject))

pythonTupleType :: [a] -> OhuaType PythonVarType res
pythonTupleType [] = defaultType
pythonTupleType (t:ts) = HType (HostType (PTuple (PythonObject :| map (const PythonObject) ts)))



setItemFunction:: Binding
setItemFunction = fromString "__setitem__"

getItemFunction::Binding
getItemFunction = fromString "__getitem__"

listConstructor::Binding
listConstructor = fromString "list_internal"

dictConstructor::Binding
dictConstructor = fromString "dict_internal"

tupleConstructor::Binding
tupleConstructor = fromString "tuple_internal"

setConstructor:: Binding
setConstructor = fromString "set_internal"

asQualified :: Binding -> QualifiedBinding
asQualified sf = QualifiedBinding (NSRef [defaultName]) sf

-- Python has no type extraction and in FR lang, method calls do not 
-- carry type information until they are resolved in the TypeSystem module.
-- Hence we need to bring the types of default methods, that won't be defined anywhere in the input 
-- into the type resolution via this pre-build Delta context.
defaultMethods :: FunTypesMap
defaultMethods = 
    let funs = HM.empty
        -- type of setItem = iterable -> index -> item -> iterable
        funs' = HM.insert (asQualified setItemFunction) (STFunType defaultType (Right (defaultType :| [defaultType])) defaultType) funs 
        -- type of getItem = iterable -> index -> item 
        funs'' = HM.insert (asQualified getItemFunction) (STFunType defaultType (Right (defaultType :| [])) defaultType) funs'
    in funs''



pattern ListConstructor :: QualifiedBinding
pattern ListConstructor <- QualifiedBinding (NSRef []) "list_internal"

pattern DictConstructor :: QualifiedBinding
pattern DictConstructor <- QualifiedBinding (NSRef []) "dict_internal"

pattern TupleConstructor::QualifiedBinding
pattern TupleConstructor <- QualifiedBinding (NSRef []) "tuple_internal"

pattern SetConstructor::QualifiedBinding
pattern SetConstructor <- QualifiedBinding (NSRef []) "set_internal"

pattern SetItemFunction::QualifiedBinding
pattern SetItemFunction <- QualifiedBinding (NSRef [defaultName]) "__setitem__"

pattern GetItemFunction::QualifiedBinding
pattern GetItemFunction <- QualifiedBinding (NSRef [defaultName]) "__getitem__"



instance Pathable PythonVarType where
    toPath (PSingle t) = Just (Left defaultName)
    toPath (PTuple ts) = Nothing 

instance Pretty PythonVarType where
    pretty (PSingle t) = "obj"
    pretty (PTuple ts) = "("<> foldr (\o s -> "obj, " <> s) "" ts <>")"

-- Most builtin Python types can be interpreted as truth values in a way
-- FIXME: If requried in any way, make this more specific (will require changes to the PythonVarType)
instance TruthableType PythonVarType where 
    isHostTruthy _pt = True

instance UnTupleType PythonVarType where
    unTupleType (PTuple ts) = NE.map PSingle ts
    unTupleType t = t:|[]

instance ListElementType PythonVarType where
    asListElementType _ = Just (PSingle PythonObject)

-- FIXME: Do we need a separate Unit type for Python?
instance TellUnitType PythonVarType where
    isHostUnit (PSingle _ ) = True
    isHostUnit _ = False 
     
type FunTypesMap = HM.HashMap QualifiedBinding (FunType PythonVarType Resolved)

data Module = Module FilePath (Py.Module Py.SrcSpan)