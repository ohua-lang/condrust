module Ohua.Integration.Python.Util where

import Ohua.Prelude

import qualified Data.Text as T

import System.FilePath
import Language.Python.Common
import qualified Language.Python.Version3 as V3
import Language.Python.Common.AST (Ident(..))


-- TODO: Do we want to support python 2? Otherwise remove and just use V3.parseModule
type Parser  = String -> String -> Either ParseError (Module SrcSpan  , [Token])

noSpan :: ()
noSpan = ()

toBinding :: Ident a -> Binding
toBinding Ident{ident_string=n, ident_annot=annot} = fromString n

fromBinding :: Binding -> annot -> Ident annot
fromBinding bnd span= Ident{ident_string=show bnd, ident_annot= span}

filePathToNsRef :: FilePath -> NSRef
filePathToNsRef = makeThrow . map fromString . splitDirectories . dropExtension

wrappedParsing:: String -> String -> Module SrcSpan
wrappedParsing pyCode filename = do
    let parseresult = V3.parseModule pyCode filename
    case parseresult of
        Left parse_error -> error $ T.pack $ prettyText parse_error
        Right (mod_span, _comments) -> mod_span


wrapExpr :: Expr () -> Statement ()
wrapExpr expr = StmtExpr expr noSpan

unwrapStmt :: Statement () -> Expr ()
unwrapStmt StmtExpr{stmt_expr=expr, stmt_annot=annot}= expr

load :: FilePath -> IO (Module SrcSpan)
load srcFile =  do
    content <- readFile srcFile
    let name =  takeFileName srcFile
    return $ wrappedParsing (T.unpack content) name