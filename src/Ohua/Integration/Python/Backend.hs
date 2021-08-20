module Ohua.Integration.Python.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

import Ohua.Integration.Lang hiding (Lang)

import Ohua.Integration.Python.Types
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeExtraction

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan)

import Data.Text (unpack)
import Data.List ((!!))
import Data.Functor.Foldable (cata, embed)

import Data.Maybe
import qualified Language.Python.Common as Py


convertToSuite::(Architecture arch, Lang arch ~ Language 'Python)
    => arch -> TaskExpr PythonTypeAnno -> Py.Suite SrcSpan
convertToSuite arch taskExpr =
    let expr = convertExpr arch taskExpr
    in [expr]

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) = PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    type Expr (Language 'Python) = Py.Statement SrcSpan
    type Task (Language 'Python) = Py.Suite SrcSpan

{--Note:  lower should basically turn a Programm (Backend.Types) of taskExpressions
     (actually a function that returns the expression inside a FullExpression) 
     into a task as defined for the Backend (in this case a Python.Suite).-}

    -- Basically converts Backend Language () to AST again but adds functionality to receive and send 
    -- local vars from to channels
    lower (Module filePath (Py.Module statements)) arch nameSpace = do
        return $
            -- Note: '&' -> forward application (reverse $), '%~'-> Setter from Lens
            -- means -> map function: algoCode of algos is set by convertTask(annotation algo)
            -- return->  ns algos, where algos are set by the map functione
            nameSpace & algos %~ map (\algo -> algo & algoCode %~ convertTasks (algo^.algoAnno))
        where
            convertTasks (Py.Fun id params opt_anno body anno) (Program chans (SRecv argType channel) tasks) =
                Program
                    chans
                    (SRecv (Type $ PythonObject noSpan) channel)
                    $ map (convertToSuite arch . convertEnvs params <$>) tasks
            convertTasks statement _ = error $ "Trying to convert something, that is not a function but "<> show statement

            convertEnvs :: [Py.Parameter a] -> TCLang.TaskExpr PythonTypeAnno -> TCLang.TaskExpr PythonTypeAnno
            convertEnvs args = cata $ \case
                LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
                e -> embed e

            argToVar :: Py.Parameter a -> TCLang.TaskExpr PythonTypeAnno
            -- argToVar Py.EndPositional{} = undefined -- they shall not pass B-)...no, actually they just not pass the frontend
            argToVar param = Var $ toBinding $ Py.param_name param

    convertExpr _ (TCLang.Var bnd) = wrapExpr Py.Var{var_ident= fromBinding bnd, expr_annot= noSpan}
    -- Question: Are there only Int literals? What about Floats ? 
    convertExpr _ (TCLang.Lit (NumericLit i)) = wrapExpr Py.Int{int_value=i, expr_literal=show i, expr_annot= noSpan}
    convertExpr _ (TCLang.Lit (BoolLit b)) = wrapExpr $ Py.Bool b noSpan
    convertExpr _ (TCLang.Lit UnitLit) = wrapExpr $ Py.None noSpan
    convertExpr _ (TCLang.Lit (EnvRefLit _hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    -- Question: What are the function IDs?
    -- It would be syntactically correct to construct a Py.Dot expression here. However it's far easier and yields the
    -- the same results, to just concatenate 
    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd mFunID _type))) = case qBnd of
        (QualifiedBinding [] bnd) -> wrapExpr Py.Var{var_ident= fromBinding bnd, expr_annot= noSpan}
        (QualifiedBinding refNames bnd)  -> wrapExpr $ toPyVar $ dotConcat refNames bnd
    
    convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
    -- There are no different definitions for functions and methods
    convertExpr arch (Apply (Stateful stateExpr (QualifiedBinding _ bnd) args)) =
        wrapExpr $
        Py.Call
            (Py.Dot{Py.dot_expr = unwrapStmt $ convertExpr arch stateExpr,
                    Py.dot_attribute = fromBinding bnd,
                    Py.expr_annot = noSpan })
            (map (convertArgument arch) args)
            noSpan
    
    convertExpr arch (TCLang.Let varName valExpr inExpr) = 
        -- This approach wont work in Python, again because I have no single Expressions or Statements,
        -- to wrap a list of statements
        -- Question: I assume this is the way, Tasks are actually structured i.e. 
        -- let y_0_0 = recv() in 
            -- let z_0_0 = recv() in
                -- do stuff
        -- > Based on this assumption I'll try to handle that case in convertToSuite
        let varAssign = convertExpr arch (TCLang.Assign varName valExpr)
            doWithIt = convertExpr arch inExpr
        in prependToSuite varAssign doWithIt
        
    convertExpr _ (TCLang.Stmt expr1 expr2) = error "Todo: I need a Stmt conversion"
    convertExpr arch (TCLang.Assign bnd expr) =
        -- Question: An equivalent to 'prependToBlock' would be pointless as long as I don't wrap function blocks into e.g. 
        -- a StmtExpr...which itself is pointless beonde the point of type compat 
        -- But what's the purpose of the TCLang.Lit UnitLit? Is it the 'return None' at the end of the produced block?
        -- In rust integration: 
            {--[Semi (Rust.Assign [] (convertExpr arch $ Var bnd) (convertExpr arch expr) noSpan) noSpan]
            $ convertExpr arch $ TCLang.Lit UnitLit --}
        Py.Assign{
                Py.assign_to = [unwrapStmt $ convertExpr arch $ Var bnd],
                Py.assign_expr = unwrapStmt $ convertExpr arch expr,
                Py.stmt_annot= noSpan}
    
    convertExpr arch (TCLang.ReceiveData recv) = convertRecv arch recv
    convertExpr arch (TCLang.SendData send) = convertSend arch send

    --- specific control flow
    convertExpr arch (TCLang.EndlessLoop expr) = error "Todo: I need an EndlessLoop conversion"
    convertExpr arch (TCLang.ForEach itemBnd itemsBnd expr) = error "Todo: I need a ForEach conversion"
    convertExpr arch (TCLang.Repeat bndOrNum expr) = error "Todo: I need a Repeat conversion"
    convertExpr arch (TCLang.While cond expr) = error "Todo: I need a While conversion"
    convertExpr arch (TCLang.Cond cond thenExp elseExp) = error "Todo: I need a Cond conversion"

    --- specific functions
    convertExpr arch (TCLang.HasSize bnd) = error "Todo: I need a HasSize conversion"
    convertExpr arch (TCLang.Size bnd) = error "Todo: I need a Size conversion"
    convertExpr arch (TCLang.ListOp listTask) = error "Todo: I need a ListOp conversion"   
    -- Todo: Actually we have nothing matching tuple in python as the backend tuple just has two components
    -- and list are homogenous :-/ 
    convertExpr arch (TCLang.Tuple one two) =
        let conv = unwrapStmt . convertExpr arch . either TCLang.Var TCLang.Lit
        in  wrapExpr $ Py.Tuple [conv one, conv two] noSpan

    convertExpr arch (TCLang.First bnd) =  wrapExpr $
        Py.Subscript{
            Py.subscriptee = toPyVar $ show bnd,
            Py.subscript_expr = Py.Int 0 "0" noSpan,
            Py.expr_annot = noSpan
        } 
    convertExpr arch (TCLang.Second bnd) = wrapExpr $
        Py.Subscript{
            Py.subscriptee = toPyVar $ show bnd,
            Py.subscript_expr = Py.Int 0 "0" noSpan,
            Py.expr_annot = noSpan
        } 

    convertExpr arch (TCLang.Increment bnd) = 
        Py.AugmentedAssign 
            (toPyVar $ show bnd) 
            (Py.PlusAssign noSpan) 
            (Py.Int 1 "1" noSpan)
            noSpan 

    convertExpr arch (TCLang.Decrement bnd) = 
        Py.AugmentedAssign 
            (toPyVar $ show bnd) 
            (Py.MinusAssign noSpan) 
            (Py.Int 1 "1" noSpan)
            noSpan 
             
    convertExpr arch (TCLang.Not expr) =  wrapExpr $
        Py.UnaryOp 
            (Py.Not noSpan) 
            (unwrapStmt $ convertExpr arch expr) 
            noSpan 

prependToSuite :: Py.Statement SrcSpan -> Py.Statement SrcSpan -> Py.Statement SrcSpan
prependToSuite = error "not implemented"



convertFunCall ::(Architecture arch, Lang arch ~ Language 'Python) =>
        arch -> QualifiedBinding -> [TCLang.TaskExpr PythonTypeAnno] -> Py.Statement SrcSpan
convertFunCall arch op [arg1, arg2] | isJust $ binOp op =
    wrapExpr $ Py.BinaryOp (fromJust $ binOp op) firstArg secondArg noSpan
    where
        firstArg = unwrapStmt $ convertExpr arch arg1
        secondArg = unwrapStmt $ convertExpr arch arg2
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
    wrapExpr $ Py.UnaryOp (fromJust $ unOp op) arg' noSpan
    where
        arg' = unwrapStmt $ convertExpr arch arg
        unOp = \case
            FunRepresentationOf "not" -> Just $ Py.Not noSpan
            FunRepresentationOf "~" -> Just $ Py.Invert noSpan
            _ -> Nothing

convertFunCall arch funRef args =
    wrapExpr $ Py.Call
                    (unwrapStmt $ convertExpr arch $ asUntypedFunctionLiteral funRef)
                    (map (convertArgument arch) args)
                    noSpan


convertArgument:: (Architecture arch, Lang arch ~ Language 'Python)=>
    arch -> TaskExpr PythonTypeAnno -> Py.Argument SrcSpan
-- TODO: Translating args and kwargs at the frontend I just prepend their names with '*'/'**'
-- > Check if translating this back just using normal args yields same behaviour
-- > Currently original type annotation and default are lost in translation (no pun intended) anyways, otherwise 
-- doing it this way would theoretically allow args/kwars with type annotation or defaults so actually this should not be 
-- 'stringly typed'
-- TODO: Keyword Arguments... Would be nice not to loose this information. 
convertArgument arch arg = Py.ArgExpr (unwrapStmt (convertExpr arch arg)) noSpan



dotConcat :: NSRef -> Binding -> String
dotConcat (NSRef refs) bind = foldr (\ref name -> show ref ++ "." ++ name) (show bind) refs

pattern FunRepresentationOf :: Binding -> QualifiedBinding
pattern FunRepresentationOf bnd <- QualifiedBinding [] bnd

asUntypedFunctionLiteral qBinding = TCLang.Lit $ FunRefLit $ FunRef qBinding Nothing Untyped

-- Turn an unqualified binding (just a name) 
-- into a qualified binding (name with [import] context) with just no context
-- TODO: The name is confusing...kept for consistency though
mkFunRefUnqual :: Binding -> QualifiedBinding
mkFunRefUnqual = QualifiedBinding (makeThrow [])



