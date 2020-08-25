module Ohua.Integration.Rust.TypeExtraction where

import Ohua.Prelude

import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Normal)
import Language.Rust.Data.Ident (Ident)
import Language.Rust.Parser (Span)
import qualified Data.HashMap.Lazy as HM

-- That could be a type family where there exists only one type for `typ` 
-- per instance of the type class that defines this method.
-- extract :: FilePath -> HS FunRef typ

data ArgType a = Self (Ty a) Mutability | Normal (Ty a) deriving (Show, Eq)

data FunType a = FunType [ArgType a] (Maybe (Ty a)) deriving (Show, Eq)

extractFromFile :: CompM m => FilePath -> m (HM.HashMap FunRef (FunType Span))
extractFromFile srcFile = extract srcFile =<< liftIO (load srcFile)

extract :: (CompM m, Show a) => FilePath -> SourceFile a -> m (HM.HashMap FunRef (FunType a))
extract srcFile (SourceFile _ _ items) = HM.fromList <$> extractTypes items
    where
        extractTypes :: (CompM m, Show a) => [Item a] -> m [(FunRef, FunType a)]
        extractTypes items = 
            catMaybes . concat <$>
            mapM
                (\case
                    (Fn _ _ ident decl _ _ _ _ _ _) -> 
                        (: []) . Just . (createFunRef ident, ) <$> extractFunType convertArg decl
                    (Impl _ _ _ _ _ _ _ selfType items _) -> 
                        mapM (extractFromImplItem selfType) items
                    -- (Trait _ _ ident _ _ _ _ items _) -> 
                    --     mapM (extractFromTraitItem (toTraitType ident)) items
                    _ -> return [])
                items
                
        -- toTraitType ident = 
        --     TraitObject 
        --         (TraitTyParamBound 
        --             (PolyTraitRef 
        --                 [] 
        --                 (TraitRef (Path False [PathSegment ident Nothing span] span) span) 
        --                 span) 
        --             None
        --             span)
        --         span
        --         :| []

        createFunRef :: Ident -> FunRef
        createFunRef = 
            flip FunRef Nothing . 
            QualifiedBinding (filePathToNsRef srcFile) .
            toBinding
        
        extractFunType :: (CompM m, Show a) => (Arg a -> m (ArgType a)) -> FnDecl a -> m (FunType a)
        extractFunType _ f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        extractFunType firstArgExtract (FnDecl args retTyp _ _) = do
            args' <- case args of 
                        [] -> return []
                        (x:xs) -> (:) <$> firstArgExtract x  <*> mapM convertArg xs
            return $ FunType args' retTyp

        convertImplArg :: (CompM m, Show a) => Ty a -> Arg a -> m (ArgType a)
        convertImplArg selfType (SelfValue mut _) = return $ Self selfType mut
        convertImplArg _ a@SelfRegion{} = throwError $ "Self arguments by reference are currently not supported." <> show a
        convertImplArg selfType (SelfExplicit _ty mut _) = return $ Self selfType mut
        convertImplArg _ a = convertArg a

        convertArg :: (CompM m, Show a) => Arg a -> m (ArgType a)
        convertArg (Arg _ typ _) = return $ Normal typ
        convertArg a = throwError $ "Please report: The impossible happened at argument: " <> show a

        extractFromImplItem :: (CompM m, Show a) => Ty a -> ImplItem a -> m (Maybe (FunRef, FunType a))
        extractFromImplItem selfType (MethodI _ _ _ ident _ (MethodSig _ _ _ decl) _ _) = 
            Just . (createFunRef ident, ) <$> extractFunType (convertImplArg selfType) decl
        extractFromImplItem _ _ = return Nothing

        extractFromTraitItem :: (CompM m, Show a) => Ty a -> TraitItem a -> m (Maybe (FunRef, FunType a))
        extractFromTraitItem selfType (MethodT _ ident _ (MethodSig _ _ _ decl) _ _) =
            Just . (createFunRef ident, ) <$> extractFunType (convertImplArg selfType) decl
        extractFromTraitItem _ _ = return Nothing