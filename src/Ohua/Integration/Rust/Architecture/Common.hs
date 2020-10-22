module Ohua.Integration.Rust.Architecture.Common where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.TypeExtraction

import Language.Rust.Syntax as Rust hiding (Rust)

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.HashMap.Lazy as HM
import System.FilePath (takeFileName)
import Language.Rust.Pretty (pretty')


serialize :: CompM m
        => Module
        -> Namespace (Program (Stmt ()) (Rust.Expr ()) (Rust.Expr ()) RustTypeAnno)
        -> (Program (Stmt ()) (Rust.Expr ()) (Rust.Expr ()) RustTypeAnno -> Block ())
        -> m (NonEmpty (FilePath, L.ByteString))
serialize (Module path (SourceFile modName atts items)) ns createProgram =         
    let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) $ ns^.algos
        src    = SourceFile modName atts $ map (replaceAlgo algos') items
        render = encodeUtf8 . (<> "\n") . renderLazy . layoutSmart defaultLayoutOptions . pretty'
        path' = takeFileName path -- TODO verify this!
    in return $ (path', render src) :| []
    where
        replaceAlgo algos = \case
                f@(Fn atts vis ident decl@(FnDecl _args _ _ _) s c abi gen _ span) ->
                    case HM.lookup (toBinding ident) algos of
                        Just algo -> 
                            Fn atts vis ident decl s c abi gen (span <$ createProgram algo) span
                        Nothing -> f
                i -> i
