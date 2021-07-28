{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

import Data.Maybe

noSpan :: ()
noSpan = ()

--Todo: I need to map TaskExprs back to Py.Statement or Py.Expr 
data PyStmtOrExpr = PyStmt (Py.Statement ()) | PyExpr (Py.Expr ()) 

convertIntoSuite::(Architecture arch, Lang arch ~ Language 'Python) 
    => arch -> TaskExpr PythonTypeAnno -> Py.Suite ()
convertIntoSuite arch taskExpr =
    let expr = convertExpr arch taskExpr
    in [expr]


instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) = PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    type Expr (Language 'Python) = Py.Expr ()
    type Task (Language 'Python) = Py.Suite ()

{--Note:  lower should basically turn a Programm (Backend.Types) of taskExpressions
     (actually a function that returns the expression inside a FullExpression) 
     into a task as defined for the Backend (in this case a Python.Suite).-}
    
    -- Basically converts Backend Language () to AST again but adds functionality to receive and send 
    -- local vars from to channels
    lower (Module filePath (Py.Module statments)) arch nameSpace = 
        return $ 
            -- Note: '&' -> forward application (reverse $), '%~'-> Setter from Lens
            -- means -> map function: algoCode of algos is set by convertTask(annotation algo)
            -- return->  ns algos, where algos are set by the map function
            ns & algos %~ map (\algo -> algo & algoCode %~ convertTasks (algo^.algoAnno))
        where
            convertTasks (Py.Fun id params opt_anno body anno) (Program chans (SRecv argType channel) tasks) =                 
                Program 
                    chans 
                    -- SRevs :: ArgType t -> Com 'Channel t -> Com 'Recv t
                    (SRecv (Type $ PythonObject noSpan) channel)
                    $ map (convertIntoSuite arch . convertEnvs args <$>) tasks 
                            
            convertEnvs :: [Py.Parameter a] -> TCLang.TaskExpr PythonTypeAnno -> TCLang.TaskExpr PythonTypeAnno
            convertEnvs args = cata $ \case
                LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
                e -> embed e

            argToVar :: Py.Parameter a -> TCLang.TaskExpr PythonTypeAnno
            argToVar Py.EndPositional{} = undefined 
            argToVar param = Var $ toBinding $ Py.param_name param
    

-- Note: Checked, spans do not matter for code generation
    convertExpr _ (TCLang.Var bnd) =  Py.Var{var_ident= fromBinding bnd noSpan, expr_annot= noSpan}
    -- Question: Are there only numeric == Int literals? What about Floats ? 
    convertExpr _ (TCLang.Lit (NumericLit i)) = Py.Int{int_value=i, expr_literal=show i, expr_annot= noSpan}
    convertExpr _ (TCLang.Lit (BoolLit b)) = Py.Bool b noSpan

    -- ToDo: Frontend converts empty calls to fun(UnitLit), while elsewhere Unit may represent None, this must
    -- not happen in function call translation because it would falsely add an argument 
    -- (actually python-internally None is passed to function calls but that's not my business here)
    convertExpr _ (TCLang.Lit UnitLit) = Py.None noSpan 
    convertExpr _ (TCLang.Lit (EnvRefLit _hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd _ _type))) = undefined

    -- Todo: Actually we have nothing matching tuple in python as the backend tuple just has two components
    -- and list are homogenous :-/ 
    convertExpr arch (TCLang.Tuple one two) = 
        let conv = convertExpr arch . either TCLang.Var TCLang.Lit
        in  Py.Tuple [conv one, conv two] noSpan

    convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
    -- There are no different definitions for functions and methods
    convertExpr arch (Apply (Stateful stateExpr (QualifiedBinding _ bnd) args)) = undefined 

    convertExpr arch (TCLang.Assign bnd expr) = undefined 

    convertExpr arch (TCLang.ReceiveData recv) = convertRecv arch recv
    convertExpr arch (TCLang.SendData send) = convertSend arch send

    
    

pattern FunRepresentationOf :: Binding -> QualifiedBinding
pattern FunRepresentationOf bnd <- QualifiedBinding [] bnd

convertFunCall :: (Architecture arch, Lang arch ~ (Language 'Python), ty ~ B.Type (Lang arch))
               => arch -> QualifiedBinding -> [TCLang.TaskExpr PythonTypeAnno] -> Py.Expr ()
convertFunCall arch op [arg1, arg2] | isJust $ binOp op = 
    Py.BinaryOp (fromJust $ binOp op) (convertExpr arch arg1) (convertExpr arch arg2) noSpan
    where
        binOp = \case
            FunRepresentationOf "+" -> Just $ Py.Plus noSpan
            FunRepresentationOf "-" -> Just $ Py.Minus noSpan
            FunRepresentationOf "*" -> Just $ Py.Multiply noSpan
            FunRepresentationOf "/" -> Just $ Py.Divide noSpan
            FunRepresentationOf "//" -> Just $ Py.FloorDivide noSpan
            FunRepresentationOf "%" -> Just $ Py.Modulo noSpan
            FunRepresentationOf "**" -> Just $ Py.Exponent noSpan
            FunRepresentationOf "@" -> Just $ Py.MatrixMult noSpan
            FunRepresentationOf "and" -> Just $ Py.And noSpan 
            FunRepresentationOf "or" -> Just $ Py.Or noSpan
            FunRepresentationOf "in" -> Just $ Py.In noSpan
            FunRepresentationOf "not in" -> Just $ Py.NotIn noSpan
            -- TODO/Question: is may not be used with literals, do I need a distinction here?
            -- >Probaly not as vars are also expressions. Python grammar referenced by l-p doesn't make a distinction
            FunRepresentationOf "is" -> Just $ Py.Is noSpan
            FunRepresentationOf "is not" -> Just $ Py.IsNot noSpan
            
            FunRepresentationOf "<" -> Just $ Py.LessThan noSpan
            FunRepresentationOf ">" -> Just $ Py.GreaterThan noSpan
            FunRepresentationOf "==" -> Just $ Py.Equality noSpan
            FunRepresentationOf ">=" -> Just $ Py.GreaterThanEquals noSpan
            FunRepresentationOf "<=" -> Just $ Py.LessThanEquals noSpan
            FunRepresentationOf "!=" -> Just $ Py.NotEquals noSpan


            FunRepresentationOf "&" -> Just $ Py.BinaryAnd noSpan
            FunRepresentationOf "|" -> Just $ Py.BinaryOr noSpan
            FunRepresentationOf "^" -> Just $ Py.Xor noSpan
            FunRepresentationOf "<<" -> Just $ Py.ShiftLeft noSpan
            FunRepresentationOf ">>" -> Just $ Py.ShiftRight noSpan
            _ -> Nothing 

convertFunCall arch op [arg] | isJust $ unOp op = 
    Py.UnaryOp (fromJust $ unOp op) (convertExpr arch arg) noSpan 
    where
        unOp = \case            
            FunRepresentationOf "not" -> Just $ Py.Not noSpan
            FunRepresentationOf "~" -> Just $ Py.Invert noSpan
            _ -> Nothing

convertFunCall arch funRef args = 
    Py.Call (convertExpr arch $ asUntypedFunctionLiteral funRef)
            (map (convertArgument arch) args)  
            noSpan


convertArgument:: (Architecture arch, Lang arch ~ (Language 'Python))=> arch -> TaskExpr PythonTypeAnno -> Py.Argument ()
-- TODO: Translating args and kwargs at the frontend I just prepend their names with '*'/'**'
-- > Check if translating this back just using normal args yields same behaviour
-- > Currently original type annotation and default are lost in translation (no pun intended) anyways, otherwise 
-- doing it this way would theoretically allow args/kwars with type annotation or defaults so actually this should not be 
-- 'stringly typed'
-- TODO: Keyword Arguments... Would be nice not to loose this information. 
convertArgument arch arg = Py.ArgExpr (convertExpr arch arg) noSpan

asUntypedFunctionLiteral qBinding = TCLang.Lit $ FunRefLit $ FunRef qBinding Nothing Untyped







