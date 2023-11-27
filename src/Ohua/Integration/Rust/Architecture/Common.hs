{-# LANGUAGE LambdaCase #-}
module Ohua.Integration.Rust.Architecture.Common where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc hiding (Pretty)
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Lazy (unpack)
import Language.Rust.Pretty (pretty', Resolve, Pretty)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Types
import qualified Ohua.Integration.Rust.Types.Extraction as TH
import Ohua.Integration.Rust.Util
import Ohua.Prelude
import System.FilePath (takeFileName)
import Language.Rust.Data.Ident

serialize ::
  ErrAndLogM m =>
  TH.Module ->
  Namespace (Program chan expr stmts TH.RustVarType) anno (OhuaType ty 'Resolved) ->
  (Program chan expr stmts TH.RustVarType -> Block ()) ->
  TH.Module ->
  m (NonEmpty (FilePath, L.ByteString))
-- REMINDER: Replace Placeholder. Output new library file
serialize (TH.Module path (SourceFile modName atts items)) ns createProgram placeholder =
  let algos' = HM.fromList $ map (\(Algo name _ty expr _ ) -> (name, expr)) $ ns ^. algos
      src = SourceFile modName atts $ map (replaceAlgo algos') items
      path' = takeFileName path -- TODO verify this!
      (TH.Module libname lib) = placeholder
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


toRustTy :: OhuaType TH.RustVarType Resolved -> Maybe (Rust.Ty ())
toRustTy ty = case ty of
    IType TypeUnit -> Just $ Rust.TupTy [] ()
    IType TypeNat -> Just $  Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "usize" Nothing ()] ()) ()
    IType TypeBool ->  Just $ Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()
    IType TypeString ->  Just $ Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "String" Nothing ()] ()) ()

    (HType (HostType (TH.Self rty _ _ ))) ->  Just rty
    (HType (HostType (TH.Normal rty))) -> Just rty

    (TType typez) ->  do
      types' <- mapM toRustTy typez
      return $ Rust.TupTy (toList types') ()

    (IType (TypeList itemType)) ->  do
      itemTy <- toRustTy itemType
      return $ PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [TypeArg itemTy] [] ())) ()] ()) ()
    FType _fty -> Nothing
