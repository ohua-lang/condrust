{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.TypeExtraction where

import Ohua.Prelude

import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Normal, Type)
import Language.Rust.Data.Ident (Ident)
import Language.Rust.Parser (Span)
import qualified Data.HashMap.Lazy as HM


data RustArgType a = Self (Ty a) (Maybe (Lifetime a)) Mutability | Normal (Ty a) deriving (Show, Eq)
type RustTypeAnno = RustArgType Span
type FunTypes = HM.HashMap QualifiedBinding (FunType RustTypeAnno)

extractFromFile :: CompM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (load srcFile)

extract :: forall m a. (CompM m, Show a) => FilePath -> SourceFile a -> m (HM.HashMap QualifiedBinding (FunType (RustArgType a)))
extract srcFile (SourceFile _ _ items) = HM.fromList <$> extractTypes items
    where
        extractTypes :: (CompM m, Show a) => [Item a] -> m [(QualifiedBinding, FunType (RustArgType a))]
        extractTypes items = 
            catMaybes . concat <$>
            mapM
                (\case
                    (Fn _ _ ident decl _ _ _ _ _ _) -> 
                        (: []) . Just . (createFunRef ident, ) <$> extractFunType (\x xs -> FunType . (:xs) <$> convertArg x) decl
                    (Impl _ _ _ _ _ _ _ selfType items _) -> 
                        mapM (extractFromImplItem selfType) items
                    (Trait _ _ ident _ _ _ _ items span) -> 
                        mapM (extractFromTraitItem (toTraitType ident span)) items
                    _ -> return [])
                items
        
        toTraitType :: Show a => Ident -> a -> Ty a
        toTraitType ident span = 
            TraitObject 
                (TraitTyParamBound 
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
        
        extractFunType :: (CompM m, Show a) => (Arg a -> [ArgType (RustArgType a)] -> m (FunType (RustArgType a))) -> FnDecl a -> m (FunType (RustArgType a))
        extractFunType _ f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        extractFunType firstArgExtract (FnDecl args _retTyp _ _) =
            case args of 
                [] -> FunType <$> mapM convertArg args -- need to do it this way to make type inference for the Show constraint happy
                (x:xs) -> firstArgExtract x  =<< mapM convertArg xs

        convertImplArg :: (CompM m, Show a) => Ty a -> Arg a -> m (ArgType (RustArgType a))
        convertImplArg selfType (SelfValue mut _) = return $ Type $ Self selfType Nothing mut
        convertImplArg selfType (SelfRegion lifeTime mut _) = return $ Type $ Self selfType lifeTime mut
        convertImplArg selfType (SelfExplicit _ty mut _) = return $ Type $ Self selfType Nothing mut
        convertImplArg _ a = convertArg a

        convertArg :: (CompM m, Show a) => Arg a -> m (ArgType (RustArgType a))
        convertArg (Arg _ typ _) = return $ Type $ Normal typ
        convertArg a = throwError $ "Please report: The impossible happened at argument: " <> show a

        extractFromImplItem :: (CompM m, Show a) => Ty a -> ImplItem a -> m (Maybe (QualifiedBinding, FunType (RustArgType a)))
        extractFromImplItem selfType (MethodI _ _ _ ident _ (MethodSig _ _ _ decl) _ _) = 
            Just . (createFunRef ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromImplItem _ _ = return Nothing

        extractFromTraitItem :: (CompM m, Show a) => Ty a -> TraitItem a -> m (Maybe (QualifiedBinding, FunType (RustArgType a)))
        extractFromTraitItem selfType (MethodT _ ident _ (MethodSig _ _ _ decl) _ _) =
            Just . (createFunRef ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromTraitItem _ _ = return Nothing

        extractFirstArg :: Ty a -> Arg a -> [ArgType (RustArgType a)] -> m (FunType (RustArgType a))
        extractFirstArg selfType x xs = 
            let funType x0 = case x0 of
                            (Type Self{}) -> STFunType x0 xs
                            _ -> FunType (x0 : xs)
            in funType <$> convertImplArg selfType x