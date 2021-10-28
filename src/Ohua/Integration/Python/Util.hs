module Ohua.Integration.Python.Util where

import Ohua.Prelude

import qualified Data.Text as T

import System.FilePath
import Language.Python.Common hiding ((<>))
import qualified Language.Python.Version3 as V3
import Language.Python.Common.AST (Ident(..))


noSpan :: SrcSpan
noSpan = SpanEmpty

toBinding :: Ident a -> Binding
toBinding Ident{ident_string=n, ident_annot=annot} = fromString n

fromBinding :: Binding -> Ident SrcSpan
fromBinding bnd = Ident{ident_string= bndToStr bnd, ident_annot= noSpan}

bndToStr :: Binding -> String 
bndToStr = T.unpack . unwrap 

toQualBinding:: String -> QualifiedBinding
toQualBinding = QualifiedBinding (makeThrow []) .fromString

mkIdent::String -> Ident SrcSpan
mkIdent name = Ident{ident_string=name, ident_annot=SpanEmpty}

toPyVar :: Ident SrcSpan -> Expr SrcSpan
toPyVar ident = Var ident noSpan

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
unwrapStmt s = error $ "unwrapStmt is not supposed to be used on " <> show s

load :: FilePath -> IO (Module SrcSpan)
load srcFile =  do
    content <- readFile srcFile
    let name =  takeFileName srcFile
    return $ wrappedParsing (T.unpack content) name

-- Todo: Remove when QQ is available
--import multiprocessing as mp
importMPStmt = Import {import_items = [ImportItem {import_item_name = [Ident {ident_string = "multiprocessing", ident_annot = noSpan}], import_as_name = Just (Ident {ident_string = "mp", ident_annot = noSpan}), import_item_annot = noSpan}], stmt_annot = noSpan}
-- tasks = []
initTasksStmt = Assign {assign_to = [Var {var_ident = Ident {ident_string = "tasks", ident_annot = noSpan}, expr_annot = noSpan}], assign_expr = List {list_exprs = [], expr_annot = noSpan}, stmt_annot = noSpan}
-- processes = []
initProcs = Assign {assign_to = [Var {var_ident = Ident {ident_string = "processes", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}], assign_expr = List {list_exprs = [], expr_annot = SpanEmpty}, stmt_annot = SpanEmpty}
{- for task in tasks:
    process = mp.Process(target=task)
    processes.append(process)-}
assignTasks = For {for_targets = [Var {var_ident = Ident {ident_string = "task", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}], for_generator = Var {var_ident = Ident {ident_string = "tasks", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, for_body = [Assign {assign_to = [Var {var_ident = Ident {ident_string = "process", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}], assign_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "mp", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "Process", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgKeyword {arg_keyword = Ident {ident_string = "target", ident_annot = SpanEmpty}, arg_expr = Var {var_ident = Ident {ident_string = "task", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, stmt_annot = SpanEmpty},StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {ident_string = "processes", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "append", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Var {var_ident = Ident {ident_string = "process", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, stmt_annot = SpanEmpty}], for_else = [], stmt_annot = SpanEmpty}
-- list(map(mp.Process.start, processes))
startProcs = StmtExpr {stmt_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "list", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "map", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Dot {dot_expr = Dot {dot_expr = Var {var_ident = Ident {ident_string = "mp", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "Process", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "start", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty},ArgExpr {arg_expr = Var {var_ident = Ident {ident_string = "processes", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, stmt_annot = SpanEmpty}

-- list(map(mp.Process.terminate, processes))
terminateProcs = StmtExpr {stmt_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "list", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "map", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Dot {dot_expr = Dot {dot_expr = Var {var_ident = Ident {ident_string = "mp", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "Process", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "terminate", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty},ArgExpr {arg_expr = Var {var_ident = Ident {ident_string = "processes", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, stmt_annot = SpanEmpty}
-- list(map(mp.Process.join, processes))
joinProcs = StmtExpr {stmt_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "list", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Call {call_fun = Var {var_ident = Ident {ident_string = "map", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, call_args = [ArgExpr {arg_expr = Dot {dot_expr = Dot {dot_expr = Var {var_ident = Ident {ident_string = "mp", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "Process", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, dot_attribute = Ident {ident_string = "join", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty},ArgExpr {arg_expr = Var {var_ident = Ident {ident_string = "processes", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, arg_annot = SpanEmpty}], expr_annot = SpanEmpty}, stmt_annot = SpanEmpty}
-- return result
returnResult :: Statement SrcSpan
returnResult= Return {return_expr = Just (Var {var_ident = Ident {ident_string = "result", ident_annot = SpanEmpty}, expr_annot = SpanEmpty}), stmt_annot = SpanEmpty}


ifNameIsMain = BinaryOp{ 
    operator = Equality{}, 
    left_op_arg = Var {var_ident = Ident {ident_string = "__name__"}}, 
    right_op_arg = Strings {strings_strings = ["'__main__'"]}}
