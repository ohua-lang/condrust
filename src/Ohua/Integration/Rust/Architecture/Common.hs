module Ohua.Integration.Rust.Architecture.Common where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Rust.Pretty (pretty')
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Types
import Ohua.Integration.Rust.TypeExtraction
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Prelude
import System.FilePath (takeFileName)

serialize ::
  CompM m =>
  Module ->
  Namespace (Program chan expr (Rust.Expr ()) RustTypeAnno) anno ->
  (Program chan expr (Rust.Expr ()) RustTypeAnno -> Block ()) ->
  m (NonEmpty (FilePath, L.ByteString))
serialize (Module path (SourceFile modName atts items)) ns createProgram =
  let algos' = HM.fromList $ map (\(Algo name expr _) -> (name, expr)) $ ns ^. algos
      src = SourceFile modName atts $ map (replaceAlgo algos') items
      render =
        encodeUtf8
          . (<> "\n")
          . renderLazy
          . layoutSmart defaultLayoutOptions
          . pretty'
      path' = takeFileName path -- TODO verify this!
   in return $ (path', render src) :| []
  where
    -- FIXME now we can just insert instead of replacing them!
    replaceAlgo algos = \case
      f@(Fn atts vis ident decl@(FnDecl _args _ _ _) header gen _ span) ->
        case HM.lookup (toBinding ident) algos of
          Just algo ->
            Fn atts vis ident decl header gen (span <$ createProgram algo) span
          Nothing -> f
      i -> i
