{-|
Module      : Python.SpecialFunctions
Description : Hosts patterns and definitions of artificially introduced functions

The functions references defined in this module are used to 'desugar' Python syntax to function
calls in the frontend and 'resugar' (:-)) them in the backend again 
-}
module Ohua.Integration.Python.SpecialFunctions where

import Ohua.Prelude


setItemFunction::Binding
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


pattern ListConstructor :: QualifiedBinding
pattern ListConstructor <- QualifiedBinding (NSRef []) "list_internal"

pattern DictConstructor :: QualifiedBinding
pattern DictConstructor <- QualifiedBinding (NSRef []) "dict_internal"

pattern TupleConstructor::QualifiedBinding
pattern TupleConstructor <- QualifiedBinding (NSRef []) "tuple_internal"

pattern SetConstructor::QualifiedBinding
pattern SetConstructor <- QualifiedBinding (NSRef []) "set_internal"

pattern SetItemFunction::QualifiedBinding
pattern SetItemFunction <- QualifiedBinding (NSRef []) "__setitem__"

pattern GetItemFunction::QualifiedBinding
pattern GetItemFunction <- QualifiedBinding (NSRef []) "__getitem__"
