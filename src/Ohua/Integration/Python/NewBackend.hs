{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ohua.Integration.Python.NewBackend where
import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

import Ohua.Integration.Lang hiding (Lang)

import Ohua.Integration.Python.Types
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeExtraction
import qualified Ohua.Integration.Python.Backend.Subset as Sub


import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan)

import Data.Text (unpack)
import Data.Functor.Foldable (cata, embed)

import Data.Maybe



convertToSuite::(Architecture arch, Lang arch ~ Language 'Python)
    => arch -> TaskExpr PythonTypeAnno -> Sub.Suite
convertToSuite arch (TCLang.Let varName valExpr inExpr) =
    convertExpr arch (TCLang.Assign varName valExpr) : convertToSuite arch inExpr
convertToSuite arch (TCLang.Stmt stmt otherStmts) =
    convertExpr arch stmt : convertToSuite arch otherStmts
convertToSuite arch taskExpr =  [convertExpr arch taskExpr]

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) = PythonArgType
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    type Expr (Language 'Python) = Sub.Stmt
    type Task (Language 'Python) = Sub.Suite

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

    convertExpr _ (TCLang.Var bnd) = wrapSubExpr $ Sub.Var bnd
    -- Question: Are there only Int literals? What about Floats ? 
    convertExpr _ (TCLang.Lit (NumericLit i)) = wrapSubExpr $ Sub.Int i
    convertExpr _ (TCLang.Lit (BoolLit b)) = wrapSubExpr $ Sub.Bool b
    convertExpr _ (TCLang.Lit UnitLit) = wrapSubExpr Sub.None
    convertExpr _ (TCLang.Lit (EnvRefLit _hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    {-
    -- Question: What are the function IDs?
    -- ! TODO: The NS references do not refere to original names but ohua namespace names 
    -}
    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd mFunID _type))) = case qBnd of
        (QualifiedBinding [] bnd) -> wrapSubExpr (Sub.Var bnd)
        (QualifiedBinding refNames bnd)  -> wrapSubExpr $ Sub.Var $ dotConcat refNames bnd

    convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
    -- There are no different definitions for functions and methods in Python
    convertExpr arch (Apply (Stateful stateBnd funBnd args)) =
        wrapSubExpr $
            Sub.Call
                (Sub.DotExpr (unwrapSubStmt $ convertExpr arch stateBnd) funBnd)
                (map (convertArgument arch) args)



    -- The Rust approach wont work in Python, again because I have no single Expressions or Statements,
    -- to wrap a list of statements
    -- Question: I assume this is the way, Tasks are actually structured i.e. 
    -- let y_0_0 = recv() in 
        -- let z_0_0 = recv() in
            -- do stuff
    -- > Based on this assumption I'll try to handle that case in convertToSuite
    convertExpr arch (TCLang.Let varName valExpr inExpr) =
       error $ "Todo: I thought I could handle this elsewhere but 'Let "<> show varName <> "' came accross"
    convertExpr _ (TCLang.Stmt expr1 expr2) = error "Todo: Stmt conversion should be handled elsewhere. Please file a bug"

    convertExpr arch (TCLang.Assign bnd expr) =
        -- Question: An equivalent to 'prependToBlock' would be pointless as long as I don't wrap function blocks into e.g. 
        -- a StmtExpr...which itself is pointless beyonde the point of type compat 
        -- But what's the purpose of the TCLang.Lit UnitLit? Is it the 'return None' at the end of the produced block?
        -- In rust integration: 
            {--[Semi (Rust.Assign [] (convertExpr arch $ Var bnd) (convertExpr arch expr) noSpan) noSpan]
            $ convertExpr arch $ TCLang.Lit UnitLit --}
        Sub.Assign
            [Sub.Var bnd]
            (unwrapSubStmt $ convertExpr arch expr)

    convertExpr arch (TCLang.ReceiveData recv) = convertRecv arch recv
    convertExpr arch (TCLang.SendData send) = convertSend arch send

    --- specific control flow
    convertExpr arch (TCLang.EndlessLoop expr) =
        let suite = convertToSuite arch expr
            condition = Sub.Bool True
        in Sub.WhileStmt condition suite

    -- Todo: Maybe rather make this a comprehension
    convertExpr arch (TCLang.ForEach itemBnd itemsBnd expr) =
        let suite = convertToSuite arch expr
            targets = [Sub.Var itemBnd]
            generator =  unwrapSubStmt $ convertExpr arch $ Var itemsBnd
        in Sub.ForStmt targets generator suite

    convertExpr arch (TCLang.Repeat bndOrNum expr) =
        let target = Sub.Var. fromString $ "_"
            endVal = case bndOrNum of
                Left bnd -> Sub.Var bnd
                Right num -> Sub.Int (fromIntegral num)
            generator = Sub.Call
                    (Sub.Var .fromString $ "range")
                    [(Sub.Arg .Sub.Int) 0, Sub.Arg endVal]
            suite = convertToSuite arch expr
        in Sub.ForStmt [target] generator suite


    convertExpr arch (TCLang.While cond expr) =
        let suite = convertToSuite arch expr
            condition = unwrapSubStmt $ convertExpr arch cond
        in Sub.WhileStmt condition suite 

    convertExpr arch (TCLang.Cond cond thenExp elseExp) =
        let ifElifs = ( unwrapSubStmt $ convertExpr arch cond, convertToSuite arch thenExp)
            elseBranch = convertToSuite arch elseExp
        in Sub.CondStmt ifElifs elseBranch

    --- specific functions
    -- TODO: hasSize = True if hasattr(bnd, '__len__') 
        -- Current solution has some pros and cons... check and change if required
    convertExpr arch (TCLang.HasSize bnd) =
        let args = hasAttrArgs bnd
        in wrapSubExpr $
            Sub.CondExpr
                (Sub.Bool True)
                (Sub.Call (Sub.Var "hasattr") args)
                (Sub.Bool False)

    convertExpr arch (TCLang.Size bnd) =
        convertFunCall arch (mkFunRefUnqual "len") [Var bnd]

    convertExpr arch (TCLang.ListOp Create) =
        convertFunCall arch (mkFunRefUnqual "list") []

    convertExpr arch (TCLang.ListOp (Append bnd expr)) =
        convertExpr arch $ Apply $ Stateful (Var bnd) (mkFunRefUnqual "append") [expr]
    -- Todo: Actually we have nothing matching tuple in python as the backend tuple just has two components
    -- and list are homogenous :-/ 
    convertExpr arch (TCLang.Tuple one two) =
        let conv =  unwrapSubStmt . convertExpr arch . either TCLang.Var TCLang.Lit
        in  wrapSubExpr $ Sub.Tuple [conv one, conv two]

    convertExpr arch (TCLang.First bnd) =  wrapSubExpr $
        Sub.TplSubscript  bnd  0
    convertExpr arch (TCLang.Second bnd) = wrapSubExpr $
        Sub.TplSubscript bnd 1

    convertExpr arch (TCLang.Increment bnd) =
        --Sub.AugmentedAssign (Sub.Var bnd) Sub.PlusAssign (Sub.Int 1)
        convertExpr arch $
            Apply $ Stateless (mkFunRefUnqual "+") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr arch (TCLang.Decrement bnd) =
        -- Sub.AugmentedAssign (Sub.Var bnd) Sub.MinusAssign (Sub.Int 1)
        convertExpr arch $
            Apply $ Stateless (mkFunRefUnqual "-") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr arch (TCLang.Not expr) =  wrapSubExpr $
        Sub.UnaryOp  Sub.Not ( unwrapSubStmt $ convertExpr arch expr)



pattern FunRepresentationOf :: Binding -> QualifiedBinding
pattern FunRepresentationOf bnd <- QualifiedBinding [] bnd

convertFunCall ::(Architecture arch, Lang arch ~ Language 'Python) =>
        arch -> QualifiedBinding -> [TCLang.TaskExpr PythonTypeAnno] -> Sub.Stmt
convertFunCall arch op [arg1, arg2] | isJust $ binOp op =
    wrapSubExpr $ Sub.BinaryOp (fromJust $ binOp op) firstArg secondArg
    where
        firstArg =  unwrapSubStmt $ convertExpr arch arg1
        secondArg =  unwrapSubStmt $ convertExpr arch arg2
        binOp = \case
            FunRepresentationOf "+" -> Just Sub.Plus
            FunRepresentationOf "-" -> Just Sub.Minus
            FunRepresentationOf "*" -> Just Sub.Multiply
            FunRepresentationOf "/" -> Just Sub.Divide
            FunRepresentationOf "//" -> Just Sub.FloorDivide
            FunRepresentationOf "%" -> Just Sub.Modulo
            FunRepresentationOf "**" -> Just Sub.Exponent
            FunRepresentationOf "@" -> Just Sub.MatrixMult
            FunRepresentationOf "and" -> Just Sub.And
            FunRepresentationOf "or" -> Just Sub.Or
            FunRepresentationOf "in" -> Just Sub.In
            FunRepresentationOf "not in" -> Just Sub.NotIn
            FunRepresentationOf "is" -> Just Sub.Is
            FunRepresentationOf "is not" -> Just Sub.IsNot

            FunRepresentationOf "<" -> Just Sub.LessThan
            FunRepresentationOf ">" -> Just Sub.GreaterThan
            FunRepresentationOf "==" -> Just Sub.Equality
            FunRepresentationOf ">=" -> Just Sub.GreaterThanEquals
            FunRepresentationOf "<=" -> Just Sub.LessThanEquals
            FunRepresentationOf "!=" -> Just Sub.NotEquals


            FunRepresentationOf "&" -> Just Sub.BinaryAnd
            FunRepresentationOf "|" -> Just Sub.BinaryOr
            FunRepresentationOf "^" -> Just Sub.Xor
            FunRepresentationOf "<<" -> Just Sub.ShiftLeft
            FunRepresentationOf ">>" -> Just Sub.ShiftRight
            _ -> Nothing

convertFunCall arch op [arg] | isJust $ unOp op =
    wrapSubExpr $ Sub.UnaryOp (fromJust $ unOp op) arg'
    where
        arg' =  unwrapSubStmt $ convertExpr arch arg
        unOp = \case
            FunRepresentationOf "not" -> Just Sub.Not
            FunRepresentationOf "~" -> Just Sub.Invert
            _ -> Nothing

convertFunCall arch funRef args =
    -- TODO: I currently do not need to remove single Unit Literals from function calls: But if functions get assigned types 
        -- later (assignTypes in the frontend), there will be an added Unit Lit in empty function calls that need to be removed
    {--let args' = case args of 
                    [TCLang.Lit UnitLit] -> []
                    _ -> map (convertArgument arch) args -}
    -- in
    wrapSubExpr $ Sub.Call
                    ( unwrapSubStmt $ convertExpr arch $ asUntypedFunctionLiteral funRef)
                    (map (convertArgument arch) args)


convertArgument:: (Architecture arch, Lang arch ~ Language 'Python)=>
    arch -> TaskExpr PythonTypeAnno -> Sub.Argument
-- TODO: Translating args and kwargs at the frontend I just prepend their names with '*'/'**'
-- > Check if translating this back just using normal args yields same behaviour
-- > Currently original type annotation and default are lost in translation (no pun intended) anyways, otherwise 
-- doing it this way would theoretically allow args/kwars with type annotation or defaults so actually this should not be 
-- 'stringly typed'
-- TODO: Keyword Arguments... Would be nice not to loose this information. 
convertArgument arch arg = Sub.Arg ( unwrapSubStmt (convertExpr arch arg))


dotConcat :: NSRef -> Binding -> Binding
dotConcat (NSRef refs) bind =
    let concatName = foldr (\ref name -> bToString ref ++ "." ++ name) (bToString bind) refs
    in fromString concatName


bToString:: Binding -> String
bToString = unpack . unwrap


asUntypedFunctionLiteral qBinding = TCLang.Lit $ FunRefLit $ FunRef qBinding Nothing Untyped

-- | Turn an unqualified binding (just a name) 
--  into a qualified binding (name with [import] context) with just no context
-- TODO: The name is confusing...kept for consistency though
mkFunRefUnqual :: Binding -> QualifiedBinding
mkFunRefUnqual = QualifiedBinding (makeThrow [])

hasAttrArgs :: Binding -> [Sub.Argument]
hasAttrArgs bnd = [Sub.Arg (Sub.Var bnd) , Sub.Arg (Sub.Strings ["'__len__'"] )]


-- | To be complient with the Integrtion class, 'convertExpr' has to return terms of type
-- 'Expr (Language 'Python)'. As the function needs to translate Statements, as well as Expressions
-- from the python AST, the later ones are wraped into and unwraped from Statements using this helpers
wrapSubExpr = Sub.StmtExpr
unwrapSubStmt (Sub.StmtExpr expr) = expr
unwrapSubStmt any = error $ "Tried to unwrap a statment other than StmtExpr " <> show any