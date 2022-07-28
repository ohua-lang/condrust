{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.TypeExtraction where

import Ohua.Prelude

import Ohua.Integration.Rust.Util

import Ohua.Types.Unit (Unit)

import Language.Rust.Syntax as Rust hiding (Normal, Type)
import Language.Rust.Data.Ident (Ident)
import Language.Rust.Parser (Span)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty


data RustArgType = Self (Ty ()) (Maybe (Lifetime ())) Mutability | Normal (Ty ()) deriving (Show, Eq)
type RustTypeAnno = RustArgType
type FunTypes = HM.HashMap QualifiedBinding (FunType RustTypeAnno)

-- | Load the given file as AST, pattern match on the content and collect
--   the types of all defined functions (fn, impl or inside trait) into a 
--   Hashmap @FunTypes@ mapping function bindings to their types
extractFromFile :: CompM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (load srcFile)

extract :: forall m a. (CompM m, Show a) => FilePath -> SourceFile a -> m (HM.HashMap QualifiedBinding (FunType RustArgType))
extract srcFile (SourceFile _ _ items) = HM.fromList <$> extractTypes items
    where
        extractTypes :: (CompM m, Show a) => [Item a] -> m [(QualifiedBinding, FunType RustArgType)]
        extractTypes items = 
            catMaybes . concat <$>
            mapM
                (\case
                    (Fn _ _ ident decl _ _ _ _ ) -> 
                        (: []) . Just . (createFunRef ident, ) <$> extractFunType (\x xs -> FunType . Right . (:|xs) <$> convertArg x) decl
                    (Impl _ _ _ _ _ _ _ selfType items _) -> 
                        mapM (extractFromImplItem selfType) items
                    (Trait _ _ ident _ _ _ _ items span) -> 
                        mapM (extractFromTraitItem (toTraitType ident span)) items
                    _ -> return [])
                items

        toTraitType :: Show a => Ident -> a -> Ty a
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

        createFunRef :: Ident -> QualifiedBinding
        createFunRef =
            QualifiedBinding (filePathToNsRef srcFile) .
            toBinding

        extractFunType :: (CompM m, Show a) =>
            (Arg a -> [ArgType RustArgType] -> m (FunType RustArgType)) ->
            FnDecl a ->
            m (FunType RustArgType)
        extractFunType _ f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        extractFunType f (FnDecl args _retTyp _ _) =
            case args of
                [] -> return $ FunType $ Left Unit
                (x:xs) -> f x  =<< mapM convertArg xs

        convertImplArg :: (CompM m, Show a) => Ty a -> Arg a -> m (ArgType RustArgType)
        convertImplArg selfType (SelfValue _ mut _) = return $ Type $ Self (void selfType) Nothing mut
        convertImplArg selfType (SelfRegion _ lifeTime mut _) = return $ Type $ Self (void selfType) (void <$>lifeTime) mut
        convertImplArg selfType (SelfExplicit _ _ty mut _) = return $ Type $ Self (void selfType) Nothing mut
        convertImplArg _ a = convertArg a

        convertArg :: (CompM m, Show a) => Arg a -> m (ArgType RustArgType)
        convertArg (Arg _ _ typ _) = return $ Type $ Normal (void typ)
        convertArg a = throwError $ "Please report: The impossible happened at argument: " <> show a

        extractFromImplItem :: (CompM m, Show a) => Ty a -> ImplItem a -> m (Maybe (QualifiedBinding, FunType RustArgType))
        extractFromImplItem selfType (MethodI _ _ _ ident _ (MethodSig _ decl) _ _) =
          case decl of
            FnDecl [] _ _ _ -> return Nothing
            _ -> Just . (createFunRef ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromImplItem _ _ = return Nothing

        extractFromTraitItem :: (CompM m, Show a) => Ty a -> TraitItem a -> m (Maybe (QualifiedBinding, FunType RustArgType ))
        extractFromTraitItem selfType (MethodT _ ident _ (MethodSig _ decl) _ _) =
            Just . (createFunRef ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromTraitItem _ _ = return Nothing

        extractFirstArg :: Ty a -> Arg a -> [ArgType RustArgType] -> m (FunType RustArgType)
        extractFirstArg selfType x xs =
            let funType x0 = case x0 of
                            (Type Self{}) -> STFunType x0 $ case xs of
                                   [] -> Left Unit
                                   (x:xs) -> Right (x :| xs)
                            _ -> FunType $ Right (x0 :| xs)
            in funType <$> convertImplArg selfType x
