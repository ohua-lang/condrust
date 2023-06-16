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
import qualified Ohua.Integration.Rust.TypeHandling  as TH
import Ohua.Integration.Rust.Util
import Ohua.Prelude
import System.FilePath (takeFileName)
import Language.Rust.Data.Ident

serialize ::
  ErrAndLogM m =>
  TH.Module ->
  Namespace (Program chan expr stmts TH.RustVarType) anno ->
  (Program chan expr stmts TH.RustVarType -> Block ()) ->
  TH.Module ->
  m (NonEmpty (FilePath, L.ByteString))
-- REMINDER: Replace Placeholder. Output new library file
serialize (TH.Module path (SourceFile modName atts items)) ns createProgram placeholder =
  let algos' = HM.fromList $ map (\(Algo name expr _ ) -> (name, expr)) $ ns ^. algos
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


toRustTy :: VarType TH.RustVarType -> Maybe (Rust.Ty ())
toRustTy ty = case ty of
    TypeUnit -> Just $ Rust.TupTy [] ()
    TypeNat -> Just $  Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "usize" Nothing ()] ()) ()
    TypeBool ->  Just $ Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()
    
    (Type (HostType (TH.Self ty _ _ ))) ->  Just $ ty
    (Type (HostType (TH.Normal ty))) -> Just $  ty
    (Type (HostType (TH.Unknown))) -> trace "Type Unknown mad it through the compiler" Just $  Rust.Infer ()
    
    (TupleTy types) ->  do 
      let check = any (== Type (HostType TH.Unknown)) types
      traceM $ "Is there an UnKnown in the types?:  " <> show check 
      types' <- mapM toRustTy types
      return $ Rust.TupTy (toList types') ()
    
    (TypeList itemType) ->  do 
      traceM $ "Got a List type"
      itemTy <- toRustTy itemType
      traceM $ "Item type is" <> show itemTy
      return $ PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [TypeArg itemTy] [] ())) ()] ()) ()
    TypeFunction (fty) -> trace ("There was a function type ") Nothing