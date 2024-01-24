{-#LANGUAGE  TypeOperators #-}
module Ohua.Integration.Python.Backend where

import Ohua.Prelude

import Ohua.Types.Vector (Nat(..), SNat(..), replicateNE, nlength, withSuccSing)

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeHandling as TH
import qualified Ohua.Integration.Python.Backend.Subset as Sub


import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan)

-- import Data.Text (unpack)
import Data.Functor.Foldable (cata, embed)

import Data.Maybe
-- import Data.List.NonEmpty ((<|))


{-| Convert a task to a function block of the
    subset Language.
-}
-- Todo: Check if Let is really something different
convertToSuite ::(Architecture arch, Lang arch ~ Language 'Python)
    => arch -> TaskExpr PythonVarType  -> Sub.Suite
convertToSuite arch (TCLang.Let varName valExpr inExpr) =
    convertExpr arch (TCLang.Assign varName valExpr) : convertToSuite arch inExpr
convertToSuite arch (TCLang.Stmt stmt otherStmts) =
    convertExpr arch stmt : convertToSuite arch otherStmts
convertToSuite arch lastStmt = case lastStmt of
    TCLang.Lit UnitLit -> []
    _ ->  [convertExpr arch lastStmt]

instance Integration (Language 'Python) where
    type HostModule (Language 'Python) = Module
    type Type (Language 'Python) = PythonVarType
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    type Expr (Language 'Python) = Sub.Stmt
    type Task (Language 'Python) = Sub.Suite

    {- | Lower basically turns a Programm (Backend.Types) of taskExpressions
         into a task as defined for the Backend (in this case a subset Suite).
    -}
    lower (Module _filePath (Py.Module _stmts)) arch nameSpace = do
        return $
            nameSpace & algos %~ map (\algo -> algo & algoCode %~ convertTasks (algo^.algoInputCode))
        where
            convertTasks (Py.Fun _id _params _opt_anno _body _anno) (Program chans (SRecv _argType channel) tasks) =
                Program
                    chans
                    (SRecv (defaultType) channel)
                    $ map (convertToSuite arch . convertEnvs <$>) tasks
            convertTasks statement _ = error $ "Trying to convert something, that is not a function but "<> show statement

            convertEnvs :: TCLang.TaskExpr PythonVarType  -> TCLang.TaskExpr PythonVarType
            convertEnvs = cata $ \case
                LitF (EnvRefLit arg _ty) -> Var arg
                e -> embed e

            -- argToVar :: Py.Parameter a -> TCLang.TaskExpr PythonVarType
            -- argToVar param = Var $ toBinding $ Py.param_name param

    convertExpr _ (TCLang.Var bnd) = wrapSubExpr $ Sub.Var bnd
    convertExpr _ (TCLang.Lit (NumericLit i)) = wrapSubExpr $ Sub.Int i
    convertExpr _ (TCLang.Lit (BoolLit b)) = wrapSubExpr $ Sub.Bool b
    convertExpr _ (TCLang.Lit (StringLit str)) = wrapSubExpr $ Sub.Strings [str]
    convertExpr _ (TCLang.Lit UnitLit) = wrapSubExpr Sub.None
    -- Question: Why is it an error?
    convertExpr _ (TCLang.Lit (EnvRefLit _hostExpr _ty)) = error "Host expression encountered! This is a compiler error. Please report!"

    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd _type))) = case qBnd of
        (QualifiedBinding (NSRef []) bnd) -> wrapSubExpr (Sub.Var bnd)
        (QualifiedBinding refNames bnd)  -> wrapSubExpr $ Sub.Var $ dotConcat refNames bnd

    convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
    convertExpr arch (Apply sOp@(Stateful stateBnd funBnd args)) = case funBnd of
        TH.SetItemFunction -> transformToSubscript arch sOp
        TH.GetItemFunction -> transformToSubscript arch sOp
        _any_statefull_function ->  wrapSubExpr $
             Sub.Call
                (Sub.DotExpr (unwrapSubStmt $ convertExpr arch stateBnd) funBnd)
                (map (convertArgument arch) args)

    convertExpr _arch (TCLang.Let varName _valExpr _inExpr) =
       error $ "Todo: I thought I could handle this elsewhere but 'Let "<> show varName <> "' came accross"
    convertExpr _ (TCLang.Stmt _expr1 _expr2) = error "Todo: Stmt conversion should be handled elsewhere. Please file a bug"

    convertExpr arch (TCLang.Assign bnd expr) =
        Sub.Assign
            [Sub.Var bnd]
            (unwrapSubStmt $ convertExpr arch expr)

    convertExpr arch (TCLang.ReceiveData recv) = convertRecv arch recv
    convertExpr arch (TCLang.SendData send) = convertSend arch send

    convertExpr arch (TCLang.EndlessLoop expr) =
        let suite = convertToSuite arch expr
            condition = Sub.Bool True
        in Sub.WhileStmt condition suite

    -- Todo: Can we find out, if the function applied 'forEach' is just a call such that
    --       we could wrap this in a comprehension ?
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

    -- TODO: hasSize = True if hasattr(bnd, '__len__') 
        -- Current solution has some pros and cons... check and change if required
    convertExpr _arch (TCLang.HasSize bnd) =
        let args = hasAttrArgs bnd
        in wrapSubExpr $
            Sub.CondExpr
                (Sub.Bool True)
                (Sub.Call (Sub.Var "hasattr") args)
                (Sub.Bool False)

    convertExpr arch (TCLang.Size bnd) =
        convertFunCall arch (toQualBinding "len") [Var bnd]

    convertExpr arch (TCLang.ListOp Create) =
        convertFunCall arch (toQualBinding TH.listConstructor) []

    convertExpr arch (TCLang.ListOp (Append bnd expr)) =
        convertExpr arch $ Apply $ Stateful (Var bnd) (toQualBinding "append") [expr]
    
    convertExpr arch (TCLang.Tuple itms) =
        let conv =  unwrapSubStmt . convertExpr arch . either TCLang.Var TCLang.Lit
        in  wrapSubExpr $ Sub.Tuple $ toList (map conv itms)

    convertExpr _arch (TCLang.Indexing bnd num) = wrapSubExpr $
        Sub.Subscript  (Sub.Var bnd) (Sub.Int num)
{-
    convertExpr arch (TCLang.First bnd) =  wrapSubExpr $
        Sub.Subscript  (Sub.Var bnd) (Sub.Int 0)
    convertExpr arch (TCLang.Second bnd) = wrapSubExpr $
        Sub.Subscript (Sub.Var bnd) (Sub.Int 1)
-}
    convertExpr arch (TCLang.Increment bnd) =
        convertExpr arch $
            Apply $ Stateless (toQualBinding "+") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr arch (TCLang.Decrement bnd) =
        convertExpr arch $
            Apply $ Stateless (toQualBinding "-") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr arch (TCLang.Not expr) =  wrapSubExpr $
        Sub.UnaryOp  Sub.Not ( unwrapSubStmt $ convertExpr arch expr)



transformToSubscript :: (Integration (Lang arch), Architecture arch, Expr (Lang arch) ~ Sub.Stmt) =>
                        arch -> App (TaskExpr (B.Type (Lang arch))) -> Sub.Stmt

-- | l.__getitem__(0) => l[0]
transformToSubscript arch (Stateful stateBnd TH.GetItemFunction [indexArg]) = 
    let subscriptee = unwrapSubStmt $ convertExpr arch stateBnd
        index = unwrapSubStmt $ convertExpr arch indexArg
    in wrapSubExpr $ Sub.Subscript subscriptee index

-- | l.__setitem(0, x) => l[0] = x
transformToSubscript arch (Stateful stateBnd TH.SetItemFunction [indexArg, itemExpr]) =
    let subscriptee = unwrapSubStmt $ convertExpr arch stateBnd
        index = unwrapSubStmt $ convertExpr arch indexArg
        assignedExpr = unwrapSubStmt $ convertExpr arch itemExpr
    in Sub.Assign [Sub.Subscript subscriptee index] assignedExpr
transformToSubscript _arch _ = error "Try to transform a function other than __getitem__ , or __setitem__ to subscripting operation"




convertFunCall ::(Architecture arch, Lang arch ~ Language 'Python) =>
        arch -> QualifiedBinding -> [TCLang.TaskExpr PythonVarType ] -> Sub.Stmt
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


convertFunCall arch TH.ListConstructor args =
    wrapSubExpr $ Sub.List items
    where items = map (unwrapSubStmt . convertExpr arch) args

convertFunCall arch TH.DictConstructor args =
    wrapSubExpr $ Sub.Dict items
    where items = map (convertDictItem arch) args

convertFunCall arch TH.TupleConstructor args =
    wrapSubExpr $ Sub.Tuple items
    where items = map (unwrapSubStmt . convertExpr arch) args

convertFunCall arch TH.SetConstructor args =
    wrapSubExpr $ Sub.Set items
    where items = map (unwrapSubStmt . convertExpr arch) args

convertFunCall arch op [arg]
    | isJust $ unOp op =
        wrapSubExpr $ Sub.UnaryOp (fromJust $ unOp op) arg'
        where
            arg' =  unwrapSubStmt $ convertExpr arch arg
            unOp = \case
                FunRepresentationOf "not" -> Just Sub.Not
                FunRepresentationOf "~" -> Just Sub.Invert
                _ -> Nothing

convertFunCall arch funRef args =
    let args' = case args of
                    [TCLang.Lit UnitLit] -> []
                    _ -> map (convertArgument arch) args
    in
      wrapSubExpr $
        Sub.Call ( unwrapSubStmt $ convertExpr arch $ asFunctionLiteral funRef (length args)) args'


convertArgument :: (Architecture arch, Lang arch ~ Language 'Python) =>
    arch -> TaskExpr PythonVarType  -> Sub.Argument
-- TODO: If I could translate args and kwargs at the frontend I'd maybe just prepend their names with '*'/'**'
-- > Check if translating this back just using normal args yields same behaviour
-- > Currently original type annotation and default are lost in translation (no pun intended) anyways, otherwise
-- doing it this way would theoretically allow args/kwars with type annotation or defaults so actually this should not be
-- 'stringly typed'
-- TODO: Keyword Arguments... Would be nice not to loose this information.
convertArgument arch arg = Sub.Arg ( unwrapSubStmt (convertExpr arch arg))

convertDictItem :: (Architecture arch, Lang arch ~ Language 'Python) =>
    arch -> TaskExpr PythonVarType  -> (Sub.Expr, Sub.Expr)
convertDictItem arch item =
    let item' = unwrapSubStmt $ convertExpr arch item
    in
        case item' of
            Sub.Var bnd -> (Sub.Subscript (Sub.Var bnd) (Sub.Int 0), Sub.Subscript (Sub.Var bnd) (Sub.Int 1))
            anyI -> error $ "dict item was not received variable but" <> show anyI


dotConcat :: NSRef -> Binding -> Binding
dotConcat (NSRef refs) bind =
    let concatName = foldr (\ref rname -> bndToStr ref ++ "." ++ rname) (bndToStr bind) refs
    in fromString concatName

asFunctionLiteral :: QualifiedBinding -> Int -> TaskExpr PythonVarType
asFunctionLiteral qBinding 0 =
  TCLang.Lit $ FunRefLit $ FunRef qBinding $ FunType (Left ()) (defaultType)
asFunctionLiteral qBinding n =
  TCLang.Lit $ FunRefLit $ FunRef qBinding $ FunType (Right (defaultType :| replicate (n-1) defaultType )) (defaultType)


hasAttrArgs :: Binding -> [Sub.Argument]
hasAttrArgs bnd = [Sub.Arg (Sub.Var bnd) , Sub.Arg (Sub.Strings ["'__len__'"] )]


-- | To be complient with the Integration class, 'convertExpr' has to return terms of type
--  'Expr (Language 'Python)'. As the function needs to translate Statements, as well as Expressions
--  from the python AST, the later ones are wraped into and unwraped from Statements using this helpers
wrapSubExpr :: Sub.Expr -> Sub.Stmt 
wrapSubExpr = Sub.StmtExpr

unwrapSubStmt :: Sub.Stmt -> Sub.Expr
unwrapSubStmt (Sub.StmtExpr expr) = expr
unwrapSubStmt anyS = error $ "Tried to unwrap a statment other than StmtExpr " <> show anyS



pattern FunRepresentationOf :: Binding -> QualifiedBinding
pattern FunRepresentationOf bnd <- QualifiedBinding (NSRef []) bnd

