module Ohua.Integration.Python.Util where

import Ohua.Prelude

import qualified Data.Text as T

import System.FilePath
import Language.Python.Common
import qualified Language.Python.Version3 as V3
import Language.Python.Common.AST (Ident(..))


noSpan :: SrcSpan 
noSpan = SpanEmpty 

toBinding :: Ident a -> Binding
toBinding Ident{ident_string=n, ident_annot=annot} = fromString n

fromBinding :: Binding -> Ident SrcSpan 
fromBinding bnd = Ident{ident_string=show bnd, ident_annot= noSpan}

mkIdent::String -> Ident SrcSpan
mkIdent name = Ident{ident_string=name, ident_annot=SpanEmpty}

filePathToNsRef :: FilePath -> NSRef
filePathToNsRef = makeThrow . map fromString . splitDirectories . dropExtension

wrappedParsing:: String -> String -> Module SrcSpan
wrappedParsing pyCode filename = do
    let parseresult = V3.parseModule pyCode filename
    case parseresult of
        Left parse_error -> error $ T.pack $ prettyText parse_error
        Right (mod_span, _comments) -> mod_span


wrapExpr :: Expr SrcSpan -> Statement SrcSpan 
wrapExpr expr = StmtExpr expr noSpan

unwrapStmt :: Statement SrcSpan  -> Expr SrcSpan 
unwrapStmt StmtExpr{stmt_expr=expr, stmt_annot=annot}= expr
unwrapStmt s = error $ "unwrapping is not supposed to be used on " Ohua.Prelude.<> show s

load :: FilePath -> IO (Module SrcSpan)
load srcFile =  do
    content <- readFile srcFile
    let name =  takeFileName srcFile
    return $ wrappedParsing (T.unpack content) name

-- Todo: Remove whe QQ is available
--import multiprocessing as mp
importMPStmt = Import {import_items = [ImportItem {import_item_name = [Ident {ident_string = "multiprocessing", ident_annot = noSpan}], import_as_name = Just (Ident {ident_string = "mp", ident_annot = noSpan}), import_item_annot = noSpan}], stmt_annot = noSpan}
-- pool = mp.Pool(mp.cpu_count())
initPoolStmt = Assign {assign_to = [Var {var_ident = Ident {ident_string = "pool", ident_annot = noSpan}, expr_annot = noSpan}], assign_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "mp", ident_annot = noSpan}, expr_annot = noSpan}, dot_attribute = Ident {ident_string = "Pool", ident_annot = noSpan}, expr_annot = noSpan}, call_args = [ArgExpr {arg_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "mp", ident_annot = noSpan}, expr_annot = noSpan}, dot_attribute = Ident {ident_string = "cpu_count", ident_annot = noSpan}, expr_annot = noSpan}, call_args = [], expr_annot = noSpan}, arg_annot = noSpan}], expr_annot = noSpan}, stmt_annot = noSpan}
-- tasks = []
initTasksStmt = Assign {assign_to = [Var {var_ident = Ident {ident_string = "tasks", ident_annot = noSpan}, expr_annot = noSpan}], assign_expr = List {list_exprs = [], expr_annot = noSpan}, stmt_annot = noSpan}
--def call(f):
--     f()
callFunStmt = Fun {fun_name = Ident {ident_string = "call", ident_annot = noSpan}, fun_args = [Param {param_name = Ident {ident_string = "f", ident_annot = noSpan}, param_py_annotation = Nothing, param_default = Nothing, param_annot = noSpan}], fun_result_annotation = Nothing, fun_body = [StmtExpr {stmt_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "f", ident_annot = noSpan}, expr_annot = noSpan}, call_args = [], expr_annot = noSpan}, stmt_annot = noSpan}], stmt_annot = noSpan}

-- pool.map(call, tasks)
poolMapStmt = StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "pool", ident_annot = noSpan}, expr_annot = noSpan}, dot_attribute = Ident {ident_string = "map", ident_annot = noSpan}, expr_annot = noSpan}, call_args = [ArgExpr {arg_expr = Var {var_ident = Ident {ident_string = "call", ident_annot = noSpan}, expr_annot = noSpan}, arg_annot = noSpan},ArgExpr {arg_expr = Var {var_ident = Ident {ident_string = "tasks", ident_annot = noSpan}, expr_annot = noSpan}, arg_annot = noSpan}], expr_annot = noSpan}, stmt_annot = noSpan}

-- pool.close()
poolCloseStmt = StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "pool", ident_annot = noSpan}, expr_annot = noSpan}, dot_attribute = Ident {ident_string = "close", ident_annot = noSpan}, expr_annot = noSpan}, call_args = [], expr_annot = noSpan}, stmt_annot = noSpan}

-- pool.join()
poolJoinStmt = StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "pool", ident_annot = noSpan}, expr_annot = noSpan}, dot_attribute = Ident {ident_string = "join", ident_annot = noSpan}, expr_annot = noSpan}, call_args = [], expr_annot = noSpan}, stmt_annot = noSpan}

ifMain = BinaryOp {operator = Equality {op_annot = noSpan}, left_op_arg = Var {var_ident = Ident {ident_string = "__name__", ident_annot = noSpan}, expr_annot = noSpan}, right_op_arg = Strings {strings_strings = ["'__main__'"], expr_annot = noSpan}, expr_annot = noSpan}


{-
Comment out when Bernie (hopefully soon) merged
importMPStmt = [pyStmt|import multiprocessing as mp|]
initPoolStmt = [pyStmt|pool = mp.Pool(mp.cpu_count())|]
initListStmt = [pyStmt|tasks = []|]
callFunStmt = [pyStmt|
def call(f):
    f()
|]
runTasksStmt = [pyStmt|pool.map(call, tasks)|]
tearDownStmt = [pyStmt|
    pool.close()
    pool.join()
|]
-}
