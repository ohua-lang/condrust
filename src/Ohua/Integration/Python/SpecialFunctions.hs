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
listConstructor = fromString "list"

dictConstructor::Binding
dictConstructor = fromString "dict"

tupleConstructor::Binding
tupleConstructor = fromString "tuple"

setConstructor:: Binding
setConstructor = fromString "set"


pattern ListConstructor :: QualifiedBinding
pattern ListConstructor <- QualifiedBinding (NSRef []) "list"

pattern DictConstructor :: QualifiedBinding
pattern DictConstructor <- QualifiedBinding (NSRef []) "dict"

pattern TupleConstructor::QualifiedBinding
pattern TupleConstructor <- QualifiedBinding (NSRef []) "tuple"

pattern SetConstructor::QualifiedBinding
pattern SetConstructor <- QualifiedBinding (NSRef []) "set"

pattern SetItemFunction::QualifiedBinding
pattern SetItemFunction <- QualifiedBinding (NSRef []) "__setitem__"

pattern GetItemFunction::QualifiedBinding
pattern GetItemFunction <- QualifiedBinding (NSRef []) "__getitem__"
