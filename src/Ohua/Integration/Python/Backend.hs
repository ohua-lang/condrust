module Ohua.Integration.Python.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

import Ohua.Integration.Lang hiding (Lang)

import Ohua.Integration.Python.Types
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeExtraction

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan, startCol)

noSpan :: ()
noSpan = ()

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) = PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    type Expr (Language 'Python) = Py.Expr ()
    type Task (Language 'Python) = Py.Suite ()

-- TODO: Check what happens to code generation without proper spans. Indentation matters so I might need to keep track of columns at least

    convertExpr _ (Var bnd) =  Py.Var{var_ident= fromBinding bnd noSpan, expr_annot= noSpan}
    -- Question: Are there only numeric == Int literals? What about Floats ? 
    convertExpr _ (TCLang.Lit (NumericLit i)) = Py.Int{int_value=i, expr_literal=show i, expr_annot= noSpan}
    convertExpr _ (TCLang.Lit (BoolLit b)) = Py.Bool b noSpan

    convertExpr _ (TCLang.Lit UnitLit) = Py.None noSpan 
    convertExpr _ (TCLang.Lit (EnvRefLit _hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd _ _type))) = undefined

    {--Note:  lower should basically turn a Programm (Backend.Types) of taskExpressions
     (actually a function that returns the expression inside a FullExpression) into a task as defined for the Backend (in this case a Python.Suite).
     Question: Can we briefly go through the types and though what exactly should happen here?-}
    {-lower :: NS (Language 'Python)
        -> arch
        -> Namespace
            (Program (Channel ty) (Com 'Recv ty) (TaskExpr ty) ty)
            (AlgoSrc (Language 'Python))
        -> m (Namespace
                (Program (Channel ty) (Com 'Recv ty) (Task (Language 'Python)) ty)
                (AlgoSrc (Language 'Python)))
    -}
    lower (Module filePath (Py.Module statments)) arch nameSpace = undefined 