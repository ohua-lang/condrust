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
import Data.Functor.Foldable (cata, embed)

import Data.Maybe



convertToSuite::(Architecture arch, Lang arch ~ Language 'Python)
    => arch -> TaskExpr PythonTypeAnno -> Py.Suite SrcSpan
convertToSuite arch (TCLang.Let varName valExpr inExpr) =
    convertExpr arch (TCLang.Assign varName valExpr) : convertToSuite arch inExpr
convertToSuite arch (TCLang.Stmt stmt otherStmts) =
    convertExpr arch stmt : convertToSuite arch otherStmts
convertToSuite arch taskExpr =  [convertExpr arch taskExpr]

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) = PythonArgType
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
                    (SRecv (Type PythonObject) channel)
                    $ map (convertToSuite arch . convertEnvs <$>) tasks
            convertTasks statement _ = error $ "Trying to convert something, that is not a function but "<> show statement

            convertEnvs :: TCLang.TaskExpr PythonTypeAnno -> TCLang.TaskExpr PythonTypeAnno
            convertEnvs = cata $ \case
                LitF (EnvRefLit arg) -> Var arg
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
    -- ! TODO: The NS references do not refere to original names but ohua namespace names 
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
        -- The Rust approach wont work in Python, again because I have no single Expressions or Statements,
        -- to wrap a list of statements
        -- Question: I assume this is the way, Tasks are actually structured i.e. 
        -- let y_0_0 = recv() in 
            -- let z_0_0 = recv() in
                -- do stuff
        -- > Based on this assumption I'll try to handle that case in convertToSuite
       error $ "Todo: I thought I could handle this elsewhere but 'Let "<> show varName <> "' came accross"

    convertExpr _ (TCLang.Stmt expr1 expr2) = error "Todo: Stmt conversion should be handled elsewhere. Please file a bug"
    convertExpr arch (TCLang.Assign bnd expr) =
        -- Question: An equivalent to 'prependToBlock' would be pointless as long as I don't wrap function blocks into e.g. 
        -- a StmtExpr...which itself is pointless beyonde the point of type compat 
        -- But what's the purpose of the TCLang.Lit UnitLit? Is it the 'return None' at the end of the produced block?
        -- In rust integration: 
            {--[Semi (Rust.Assign [] (convertExpr arch $ Var bnd) (convertExpr arch expr) noSpan) noSpan]
            $ convertExpr arch $ TCLang.Lit UnitLit --}
        Py.Assign{
                Py.assign_to = [unwrapStmt $ convertExpr arch $ Var bnd],
                Py.assign_expr = unwrapStmt $ convertExpr arch expr ,--unwrapStmt $ convertExpr arch expr,
                Py.stmt_annot= noSpan}

    convertExpr arch (TCLang.ReceiveData recv) = convertRecv arch recv
    convertExpr arch (TCLang.SendData send) = convertSend arch send

    --- specific control flow
    convertExpr arch (TCLang.EndlessLoop expr) =
        let suite = convertToSuite arch expr
            elseClause = []
            condition = Py.Bool True noSpan
        in Py.While condition suite elseClause noSpan

    -- Todo: Maybe rather make this a comprehension
    convertExpr arch (TCLang.ForEach itemBnd itemsBnd expr) =
        let suite = convertToSuite arch expr
            targets = [toPyVar .fromBinding $ itemBnd]
            generator = unwrapStmt $ convertExpr arch $ Var itemsBnd
            elseSuite = []
        in Py.For targets generator suite elseSuite noSpan

    convertExpr arch (TCLang.Repeat bndOrNum expr) = 
        let target = toPyVar . mkIdent $ "_"
            endVal = case bndOrNum of
                Left bnd -> toPyVar . fromBinding $ bnd 
                Right num -> pyInt (fromIntegral num)
            generator = Py.Call 
                    (toPyVar . mkIdent $ "range") 
                    [Py.ArgExpr (pyInt 0) noSpan, Py.ArgExpr endVal noSpan] 
                    noSpan
            suite = convertToSuite arch expr
            elseSuite = []
        in Py.For [target] generator suite elseSuite noSpan


    convertExpr arch (TCLang.While cond expr) =
        let suite = convertToSuite arch expr
            condition = unwrapStmt $ convertExpr arch cond
            elseSuite = []
        in Py.While condition suite elseSuite noSpan

    convertExpr arch (TCLang.Cond cond thenExp elseExp) =
        let ifElifs = [(unwrapStmt $ convertExpr arch cond, convertToSuite arch thenExp)]
            elseBranch = convertToSuite arch elseExp
        in Py.Conditional ifElifs elseBranch noSpan

    --- specific functions
    -- TODO: hasSize = True if hasattr(bnd, '__len__') 
        -- Current solution has some pros and cons... check and change if required
    convertExpr arch (TCLang.HasSize bnd) = 
        let args = hasAttrArgs bnd
        in wrapExpr $
            Py.CondExpr
                (Py.Bool True noSpan)
                (Py.Call (toPyVar. mkIdent $ "hasattr") args noSpan )
                (Py.Bool False noSpan)
                noSpan

    convertExpr arch (TCLang.Size bnd) = 
        convertExpr arch $ Apply $ Stateless (mkFunRefUnqual "len") [Var bnd]

    convertExpr arch (TCLang.ListOp Create) = 
        convertExpr arch $ Apply $ Stateless (mkFunRefUnqual "list") []

    convertExpr arch (TCLang.ListOp (Append bnd expr)) = 
        convertExpr arch $ Apply $ Stateful (Var bnd) (mkFunRefUnqual "append") [expr]
    -- Todo: Actually we have nothing matching tuple in python as the backend tuple just has two components
    -- and list are homogenous :-/ 
    convertExpr arch (TCLang.Tuple one two) =
        let conv = unwrapStmt . convertExpr arch . either TCLang.Var TCLang.Lit
        in  wrapExpr $ Py.Tuple [conv one, conv two] noSpan

    convertExpr arch (TCLang.First bnd) =  wrapExpr $
        Py.Subscript{
            Py.subscriptee = toPyVar $ fromBinding bnd,
            Py.subscript_expr = pyInt 0,
            Py.expr_annot = noSpan
        }
    convertExpr arch (TCLang.Second bnd) = wrapExpr $
        Py.Subscript{
            Py.subscriptee = toPyVar $ fromBinding bnd,
            Py.subscript_expr = pyInt 1,
            Py.expr_annot = noSpan
        }

    convertExpr arch (TCLang.Increment bnd) =
        Py.AugmentedAssign
            (toPyVar $ fromBinding bnd)
            (Py.PlusAssign noSpan)
            (pyInt 1)
            noSpan

    convertExpr arch (TCLang.Decrement bnd) =
        Py.AugmentedAssign
            (toPyVar $ fromBinding bnd)
            (Py.MinusAssign noSpan)
            (pyInt 1) 
            noSpan

    convertExpr arch (TCLang.Not expr) =  wrapExpr $
        Py.UnaryOp
            (Py.Not noSpan)
            (unwrapStmt $ convertExpr arch expr)
            noSpan


pattern FunRepresentationOf :: Binding -> QualifiedBinding
pattern FunRepresentationOf bnd <- QualifiedBinding [] bnd

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
    -- TODO: I currently do not need to remove single Unit Literals from function calls: But if functions get assigned types 
        -- later (assignTypes in the frontend), there will be an added Unit Lit in empty function calls that need to be removed
    {--let args' = case args of 
                    [TCLang.Lit UnitLit] -> []
                    _ -> map (convertArgument arch) args -}
    -- in
    wrapExpr $ Py.Call
                    (unwrapStmt $ convertExpr arch $ asUntypedFunctionLiteral funRef)
                    (map (convertArgument arch) args) -- args'
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

dotConcat :: NSRef -> Binding -> Py.Ident SrcSpan
dotConcat (NSRef refs) bind =
    let concatName = foldr (\ref name -> bToString ref ++ "." ++ name) (bToString bind) refs
    in mkIdent concatName


bToString:: Binding -> String
bToString = unpack . unwrap


asUntypedFunctionLiteral qBinding = TCLang.Lit $ FunRefLit $ FunRef qBinding Nothing Untyped

-- Turn an unqualified binding (just a name) 
-- into a qualified binding (name with [import] context) with just no context
-- TODO: The name is confusing...kept for consistency though
mkFunRefUnqual :: Binding -> QualifiedBinding
mkFunRefUnqual = QualifiedBinding (makeThrow [])

hasAttrArgs :: Binding -> [Py.Argument SrcSpan]
hasAttrArgs bnd = [Py.ArgExpr (toPyVar. fromBinding $ bnd) noSpan, Py.ArgExpr (Py.Strings ["'__len__'"] noSpan) noSpan]

pyInt::Integer -> Py.Expr SrcSpan
pyInt num = Py.Int num (show num) noSpan
