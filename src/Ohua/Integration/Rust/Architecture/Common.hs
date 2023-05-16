module Ohua.Integration.Rust.Architecture.Common where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc hiding (Pretty)
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Lazy (unpack)
import Language.Rust.Pretty (pretty', Resolve, Pretty)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Types
import qualified Ohua.Integration.Rust.TypeExtraction  as TE
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Prelude
import System.FilePath (takeFileName)
import Language.Rust.Data.Ident

serialize ::
  CompM m =>
  Module ->
  Namespace (Program chan expr stmts TE.RustTypeAnno) anno  TE.RustTypeAnno ->
  (Program chan expr stmts TE.RustTypeAnno -> Block ()) ->
  Module ->
  m (NonEmpty (FilePath, L.ByteString))
-- REMINDER: Replace Placeholder. Output new library file
serialize (Module path (SourceFile modName atts items)) ns createProgram placeholder =
  let algos' = HM.fromList $ map (\(Algo name expr _ _) -> (name, expr)) $ ns ^. algos
      src = SourceFile modName atts $ map (replaceAlgo algos') items
      path' = takeFileName path -- TODO verify this!
      (Module libname lib) = placeholder 
   in return $ (path', render src) :| [(libname, render lib)]
  where
    -- FIXME now we can just insert instead of replacing them!
    replaceAlgo algos = \case
      f@(Fn atts vis ident decl@(FnDecl _args _ _ _) header gen _ span) ->
        case HM.lookup (toBinding ident) algos of
          Just algo ->
            Fn atts vis ident decl header gen (span <$ createProgram algo) span
          Nothing -> f
      i -> i

render :: (Resolve a, Pretty a) => a -> L.ByteString
render =
  encodeUtf8
    . (<> "\n")
    . renderLazy
    . layoutSmart defaultLayoutOptions
    . pretty'

renderStr :: (Resolve a, Pretty a) => a -> String
renderStr =
    unpack
    . renderLazy
    . layoutSmart defaultLayoutOptions
    . pretty'


toRustTy :: VarType TE.RustTypeAnno -> Rust.Ty ()
-- ToDo: We have a distinction between 'single' types and tuples but beyond that do not care
-- if it's a Path expression a Self or whatever. Currently we don't allow fancy return types so
-- maybe that's Ok but I have to evaluate later!!
toRustTy TypeVar = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "_" Nothing ()] ()) ()
toRustTy (Type (TE.Self ty _ _ )) = ty
toRustTy (Type (TE.Normal ty)) = ty
toRustTy (TupleTy types) = Rust.TupTy (toList $ map toRustTy types) ()
toRustTy TypeNat = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "usize" Nothing ()] ()) ()
toRustTy TypeBool = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()
toRustTy TypeUnit = Rust.TupTy [] ()
toRustTy (TypeList itemType) =  PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [TypeArg (toRustTy itemType)] [] ())) ()] ()) ()
  
