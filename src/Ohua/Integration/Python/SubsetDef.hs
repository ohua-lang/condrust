module Ohua.Integration.Python.SubsetDef where

import Ohua.Commons.Prelude
import qualified Language.Python.Common.AST as Py

import Data.Data

newtype PyModule  = PyModule [AnyStatement] 
data AnyStatement = TopLevelStatement | AnyLevelStatement

data TopLevelStatement ty 
    = Import [ImportItem ty]
    | ImportFrom 
    | FunDef  
        { fun_name :: Py.Ident ty -- ^ Function name.
        , fun_args :: [Py.Parameter ty] -- ^ Function parameter list.
        , fun_result_annotation :: Maybe (Expr ty) -- ^ Optional result annotation.
        , fun_body :: Body ty -- ^ Function body.
        } 
    | Class ty
    
data AnyLevelStatement ty
    = Assign ty 
    | While ty
    | For ty
    | Statement ty
    | ITE ty
    | Return ty
    | Pass ty

--actually the arg is a Span, not a type annotation but why not reuse it
type Ident = Py.Ident
type ImportItem ty = Py.ImportItem ty 
-- Note targets of assignments (or for-loops) can be just listed items in python and obvioulsy also in the IR (Let [Pat])
-- but not in haskell where unpacking requires resembling the right side structure 
type ImportFrom ty = Py.FromImport ty
type Body ty = [AnyLevelStatement ty]
type Expr ty = Py.Expr ty

data Parameter annot
   -- | Ordinary named parameter.
   = Param
     { param_name :: Ident annot -- ^ Parameter name.
     , param_py_annotation :: Maybe (Expr annot) -- ^ Optional annotation.
     , param_default :: Maybe (Expr annot) -- ^ Optional default value.
     , param_annot :: annot
     }
   {-- | Excess positional parameter (single asterisk before its name in the concrete syntax). 
   | VarArgsPos
     { param_name :: Ident annot -- ^ Parameter name.
     , param_py_annotation :: Maybe (Expr annot) -- ^ Optional annotation.
     , param_annot :: annot
     }
   -- | Excess keyword parameter (double asterisk before its name in the concrete syntax).
   | VarArgsKeyword
     { param_name :: Ident annot -- ^ Parameter name.
     , param_py_annotation :: Maybe (Expr annot) -- ^ Optional annotation.
     , param_annot :: annot
     }
   -- | Marker for the end of positional parameters (not a parameter itself).
   | EndPositional { param_annot :: annot }
   -- | Tuple unpack. /Version 2 only/.
   | UnPackTuple
     { param_unpack_tuple :: ParamTuple annot -- ^ The tuple to unpack.
     , param_default :: Maybe (Expr annot) -- ^ Optional default value.
     , param_annot :: annot
     }--}
   deriving (Eq,Ord,Show,Typeable,Functor)




