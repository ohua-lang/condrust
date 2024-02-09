{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Ohua.Integration.Python.Util where

import Ohua.Commons.Prelude
import Language.Python.Common hiding ((<>))
import Language.Python.Common.AST (Ident(..))
import qualified Language.Python.Version3 as V3


import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

noSpan :: SrcSpan
noSpan = SpanEmpty

toBinding :: Ident a -> Binding
toBinding Ident{ident_string=n, ident_annot=annot} = fromString n

fromBinding :: Binding -> Ident SrcSpan
fromBinding bnd = Ident{ident_string= bndToStr bnd, ident_annot= noSpan}

bndToStr :: Binding -> String
bndToStr = T.unpack . unwrap

-- | Turn an unqualified binding (just a name) 
--   into a qualified binding (name with [import] context) with just no context
toQualBinding:: Binding -> QualifiedBinding
toQualBinding = QualifiedBinding (makeThrow [])

mkIdent::String -> Ident SrcSpan
mkIdent name = Ident{ident_string=name, ident_annot=SpanEmpty}

toPyVar :: Ident SrcSpan -> Expr SrcSpan
toPyVar ident = Var ident noSpan

convertQBind :: QualifiedBinding -> Ident SrcSpan
convertQBind (QualifiedBinding _ bnd) = fromBinding bnd



unsupPyError :: (MonadError Text m, Pretty a1) => Text -> a1 -> m a2
unsupPyError text expr = throwError $ "Currently we do not support "<> text <>" used in: "<> (T.pack. prettyText $ expr)

py2Error expr = throwError $ "For whatever reason you managed to get the exclusively version 2 expression "
                                <> show expr <> " through the python3 parser of language-python."


encodePretty :: Module SrcSpan -> L.ByteString
encodePretty = encodeUtf8 . prettyText

filePathToNsRef :: FilePath -> NSRef
filePathToNsRef = makeThrow . filePathToList

filePathToList :: FilePath -> [Binding]
filePathToList = map fromString . splitDirectories . dropExtension

wrappedParsing:: String -> String -> Module SrcSpan
wrappedParsing pyCode filename = do
    let parseresult = V3.parseModule pyCode filename
    case parseresult of
        Left parse_error -> error $ T.pack $ prettyText parse_error
        Right (mod_span, _comments) -> mod_span


wrapExpr :: Expr SrcSpan -> Statement SrcSpan
wrapExpr expr = StmtExpr expr noSpan

unwrapStmt :: Statement SrcSpan -> Expr SrcSpan
unwrapStmt StmtExpr{stmt_expr=expr, stmt_annot=annot}= expr
unwrapStmt s = error $ "unwrapStmt is not supposed to be used on " <> show s

load :: FilePath -> IO (Module SrcSpan)
load srcFile =  do
    content <- readFile srcFile
    let name =  takeFileName srcFile
    return $ wrappedParsing (T.unpack content) name

-- Todo: Remove when QQ is available
--import multiprocessing as mp
importMPStmt = Import [ImportItem [Ident  "multiprocessing" noSpan]   (Just (Ident "mp" noSpan)) noSpan] noSpan

-- processes = []
initProcs = Assign  [Var (Ident "processes" noSpan) noSpan ]
                    (List [] noSpan)
                    noSpan

{- for task, channels in zip(tasks, channels):
    process = mp.Process(target=task, args=channels)
    processes.append(process)-}
assignTasks = For   [Var ( Ident  "task" noSpan) noSpan, Var ( Ident  "channels" noSpan) noSpan]
                    (Call ( Var (Ident "zip" noSpan) noSpan) 
                            [ArgExpr ( Var ( Ident "tasks"noSpan) noSpan) noSpan,
                             ArgExpr ( Var ( Ident "channels"noSpan) noSpan) noSpan ] 
                             noSpan)
                    [
                        Assign   [Var (Ident "process" noSpan) noSpan ]
                              (Call ( Dot (Var (Ident "mp" noSpan) noSpan) (Ident "Process" noSpan) noSpan)
                                    [ ArgKeyword (Ident "target" noSpan) (Var (Ident "task" noSpan) noSpan) noSpan,
                                      ArgKeyword (Ident "args" noSpan) (Var (Ident "channels" noSpan) noSpan) noSpan]
                                    noSpan)
                              noSpan,

                        StmtExpr ( Call ( Dot (Var ( Ident "processes" noSpan) noSpan) (Ident "append" noSpan) noSpan)
                                            [ArgExpr ( Var ( Ident "process"noSpan) noSpan) noSpan ]
                                             noSpan )
                                    noSpan
                    ] [] noSpan

-- list(map(mp.Process.start, processes))
startProcs = StmtExpr   (Call ( Var ( Ident "list"  noSpan )  noSpan )  
                                [ArgExpr ( Call ( Var ( Ident  "map" noSpan )  noSpan )
                                                [ArgExpr ( Dot ( Dot (Var (Ident "mp" noSpan )  noSpan ) (Ident  "Process" noSpan )  noSpan ) (Ident "start" noSpan )  noSpan ) noSpan,
                                                 ArgExpr ( Var ( Ident  "processes" noSpan )  noSpan ) noSpan 
                                                ]
                                                noSpan ) noSpan
                                ]
                                noSpan )
                        noSpan

-- list(map(mp.Process.terminate, processes))
terminateProcs = StmtExpr   (Call ( Var ( Ident "list"  noSpan )  noSpan )  
                                [ArgExpr ( Call ( Var ( Ident  "map" noSpan )  noSpan ) 
                                                [ArgExpr ( Dot ( Dot (Var (Ident "mp" noSpan )  noSpan ) (Ident  "Process" noSpan )  noSpan ) (Ident "terminate" noSpan )  noSpan ) noSpan,
                                                 ArgExpr ( Var ( Ident  "processes" noSpan )  noSpan ) noSpan 
                                                ]
                                                noSpan ) noSpan
                                ]
                                noSpan )
                        noSpan
-- list(map(mp.Process.join, processes))
joinProcs = StmtExpr   (Call ( Var ( Ident "list"  noSpan )  noSpan )  
                                [ArgExpr ( Call ( Var ( Ident  "map" noSpan )  noSpan ) 
                                                [ArgExpr ( Dot ( Dot (Var (Ident "mp" noSpan )  noSpan ) (Ident  "Process" noSpan )  noSpan ) (Ident "join" noSpan )  noSpan ) noSpan,
                                                 ArgExpr ( Var ( Ident  "processes" noSpan )  noSpan ) noSpan 
                                                ]
                                                noSpan ) noSpan
                                ]
                                noSpan )
                        noSpan

-- return result
returnResult :: Statement SrcSpan
returnResult= Return (Just (Var ( Ident "result" noSpan) noSpan)) noSpan

-- REMINDER: Remove this when real function extraction is implemented
placeholderModule :: Module SrcSpan
placeholderModule = Module [importMPStmt] 