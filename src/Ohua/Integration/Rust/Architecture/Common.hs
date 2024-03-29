{-# LANGUAGE LambdaCase #-}
module Ohua.Integration.Rust.Architecture.Common where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc hiding (Pretty)
import Data.Text.Prettyprint.Doc.Render.Text

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Parser (Span)

import Ohua.Backend.Types
import qualified Ohua.Integration.Rust.Types.Extraction as TH
import Ohua.Integration.Rust.Util
import Ohua.Commons.Prelude
import System.FilePath (takeFileName)


serialize :: 
  ErrAndLogM m 
  => TH.Module 
  -> Namespace (Program chan expr stmts (Rust.Expr Span) TH.RustVarType) anno (OhuaType ty 'Resolved) 
  -> (Program chan expr stmts (Rust.Expr Span) TH.RustVarType -> Block ()) 
  -> TH.Module 
  -> m (NonEmpty (FilePath, L.ByteString))
-- REMINDER: Replace Placeholder. Output new library file
serialize (TH.Module path (SourceFile modName atts items)) ns' createProgram placeholder =
  let algos' = HM.fromList $ map (\(Algo aName _ty expr _ ) -> (aName, expr)) $ ns' ^. algos
      src = SourceFile modName atts $ map (replaceAlgo algos') items
      path' = takeFileName path -- TODO verify this!
      (TH.Module libname lib) = placeholder
   in return $ (path', render src) :| [(libname, render lib)]
  where
    -- FIXME now we can just insert instead of replacing them!
    replaceAlgo algs = \case
      f@(Fn attrs vis ident decl@(FnDecl _args _ _ _) header gen _ span) ->
        case HM.lookup (toBinding ident) algs of
          Just algo ->
            Fn attrs vis ident decl header gen (span <$ createProgram algo) span
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
