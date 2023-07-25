{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ohua.Integration.Rust.Types.Extraction where

import Ohua.Prelude

import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.Common.Subset as Sub (TyRef (..), CallRef (..), GenericArgs ( Parenthesized ) )
import Ohua.Integration.Rust.Frontend.Convert (convertTy, convertPath)

import Ohua.Types.Unit (Unit)

import Language.Rust.Syntax as Rust hiding (Normal, Type)
import Language.Rust.Data.Ident (Ident)
import Language.Rust.Parser (Span, parse, inputStreamFromString)
import Language.Rust.Pretty as RustP

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.Prettyprint.Doc as Doc (Pretty(..))
import Data.List.NonEmpty hiding (map)
import Data.Text (pack)


data Module = Module FilePath (SourceFile Span)

data RustVarType = Self (Ty ()) (Maybe (Lifetime ())) Mutability | Normal (Ty ()) deriving (Show, Eq)
type RustHostType = HostType RustVarType
type FunTypes = HM.HashMap QualifiedBinding (FunType RustVarType)


instance Doc.Pretty RustVarType where
    pretty (Self ty lT  mut) = RustP.pretty' ty
    pretty (Normal ty) = RustP.pretty' ty

rustUnitReturn :: Ty ()
rustUnitReturn = Rust.TupTy [] () -- Nothing (Rust.Path False [Rust.PathSegment "()" Nothing ()] ()) ()

rustBool :: Ty ()
rustBool = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()

rustI32 :: Ty ()
rustI32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()

hostReturnSelf :: VarType RustVarType
hostReturnSelf = Type $ HostType $ Normal $ PathTy Nothing (Path False [PathSegment "Self" Nothing ()] ()) ()

rustInfer :: Ty ()
rustInfer = Infer ()

asHostNormal :: Ty a -> VarType RustVarType
asHostNormal ty = Type $ HostType $ Normal (deSpan ty)

asHostSelf :: Ty a -> (Maybe (Lifetime a)) -> Mutability-> VarType RustVarType
asHostSelf ty lt mut = Type $ HostType $ Self (deSpan ty) (map deSpan lt) mut


-- | Load the given file as AST, pattern match on the content and collect
--   the types of all defined functions (fn, impl or inside trait) into a
--   Hashmap @FunTypes@ mapping function bindings to their types

extractFromFile :: ErrAndLogM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (loadRustFile srcFile)

extract :: forall m.ErrAndLogM m => FilePath -> SourceFile Span -> m (HM.HashMap QualifiedBinding (FunType RustVarType))
extract srcFile (SourceFile _ _ items) = HM.fromList <$> extractTypes items
    where
        extractTypes :: [Item Span] -> m [(QualifiedBinding, FunType RustVarType)]
        extractTypes items =
            catMaybes . concat <$>
            mapM
                (\case
                    (Fn atts _ ident decl _ _ _ _ ) -> do
                        path <- getPath atts
                        let fName = QualifiedBinding path $ toBinding ident
                        (argTys, retTy) <- getTypes decl
                        let fType =  FunType argTys retTy
                        return (Just (fName, fType): [])
                    (Impl atts _ _ _ _ _ _ selfType items _) -> do
                        path <- getPath atts
                        selfTyRef <- convertTy selfType
                        let path' = prependNS path selfTyRef
                        mapM (extractFromImplItem path' selfType) items
                    (Trait atts _ ident _ _ _ _ items span) -> do
                        (NSRef path) <- getPath atts
                        let path' = NSRef $ path ++ [toBinding ident]
                        mapM (extractFromTraitItem path' (toTraitType ident span)) items
                    _ -> return [])
                items

        prependNS (NSRef path1) (TyRef (QualifiedBinding (NSRef path2) bnd2) _) =
            NSRef $ path1 ++ path2 ++ [bnd2]

        getPath :: [Attribute Span] -> m NSRef
        getPath atts = do
            externSpecs <- catMaybes <$> mapM checkExternSpec atts
            case externSpecs of
                [] -> return $ makeThrow $ filePathToList srcFile
                [QualifiedBinding (NSRef path) bnd] -> return $ NSRef $ path ++ [bnd]
                xs -> throwError $ "Detected multiple extern_spec definitions. Please only specify one: " <> show xs

        checkExternSpec :: Attribute Span -> m (Maybe QualifiedBinding)
        checkExternSpec (Attribute Outer path ts _) = do
            path' <- convertPath path
            case path' of
                Left "extern_spec" ->
                  case tokenStreamToExpr ts of
                    Right (ParenExpr _ (PathExpr _ Nothing nsPath _) _) -> do
                      nsPath' <- convertPath nsPath
                      case nsPath' of
                        Left bnd -> return $ Just $ QualifiedBinding (makeThrow []) bnd
                        Right (Sub.CallRef qbnd _) -> return $ Just qbnd
                    _ -> return Nothing
                _ -> return Nothing
        checkExternSpec _ = return Nothing

        tokenStreamToExpr = parse @(Rust.Expr Span) . inputStreamFromString . renderStr

        toTraitType :: Ident -> Span -> Ty Span
        toTraitType ident span =
            TraitObject
                (TraitBound
                    (PolyTraitRef
                        []
                        (TraitRef $ Path False [PathSegment ident Nothing span] span)
                        span)
                    None
                    span
                :| [])
                span

        createRef :: NSRef -> Ident -> QualifiedBinding
        createRef path funIdent = QualifiedBinding path $ toBinding funIdent

        getTypes :: FnDecl Span -> m (NonEmpty (VarType RustVarType), VarType RustVarType)
        getTypes f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        getTypes (FnDecl [] retType _ _) = return (TypeUnit :| [], fromMaybeRet retType)
        getTypes (FnDecl (a:args) retType _ _) = return (map toVarType (a:|args), fromMaybeRet retType)

        fromMaybeRet:: Maybe (Ty Span) -> VarType RustVarType
        fromMaybeRet (Just retTy) = asHostNormal retTy
        fromMaybeRet Nothing = asHostNormal rustUnitReturn

        extractFunType ::
            (Arg Span -> [VarType RustVarType] -> VarType RustVarType -> m (FunType RustVarType)) ->
            FnDecl Span ->
            m (FunType RustVarType)
        extractFunType _ f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        extractFunType f (FnDecl args retType _ _) =
            case args of
                [] -> return $ FunType (TypeUnit :| []) (fromMaybeRet retType)
                (fstArg : args) -> f fstArg (map toVarType args) (fromMaybeRet retType)

        convertImplArg :: Ty Span -> Arg Span -> VarType RustVarType
        convertImplArg selfType (SelfValue _ mut _) = asHostSelf selfType Nothing mut
        convertImplArg selfType (SelfRegion _ lifeTime mut _) = asHostSelf selfType lifeTime mut -- Type $ Self (void selfType) (void <$>lifeTime) mut
        convertImplArg selfType (SelfExplicit _ _ty mut _) = asHostSelf selfType Nothing mut
        convertImplArg _ (Arg _ _ typ _) = asHostNormal typ

        toVarType :: Arg Span -> VarType RustVarType
        toVarType (Arg _ _ typ _) = asHostNormal typ
        toVarType a = error $ "The impossible happened. Self Type outside of struct of trait discovered: " <> show a

        extractFromImplItem :: NSRef -> Ty Span -> ImplItem Span -> m (Maybe (QualifiedBinding, FunType RustVarType))
        extractFromImplItem path selfType (MethodI _ _ _ ident _ (MethodSig _ decl) _ _) =
            Just . (createRef path ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromImplItem _ _ _ = return Nothing

        extractFromTraitItem :: NSRef -> Ty Span -> TraitItem Span -> m (Maybe (QualifiedBinding, FunType RustVarType ))
        extractFromTraitItem path selfType (MethodT _ ident _ (MethodSig _ decl) _ _) =
            Just . (createRef path ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromTraitItem _ _ _ = return Nothing

        extractFirstArg :: Ty Span -> Arg Span -> [VarType RustVarType] -> VarType RustVarType -> m (FunType RustVarType)
        extractFirstArg selfType fstArg args retTy =
            -- Replace a return type Self with the actual name of the struct
            let actualReturnType = if retTy == hostReturnSelf then asHostNormal selfType else retTy
                funType x0 = case x0 of
                              (Type (HostType Self{})) -> STFunType x0 args actualReturnType
                              _ -> FunType (x0 :| args) actualReturnType
            in return $ funType $ convertImplArg selfType fstArg

