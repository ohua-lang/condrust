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

data ArgType = Self (Ty Span) Mutability | Normal (Ty Span)

data FnType = FnType [ArgType] (Maybe (Ty Span))

extract :: CompM m => FilePath -> m (HM.HashMap FunRef FnType)
extract srcFile = do
    mod <- liftIO $ load srcFile
    extractTypes mod
    where
        extractTypes :: CompM m => SourceFile Span -> m (HM.HashMap FunRef FnType)
        extractTypes (SourceFile _ _ items) = 
            HM.fromList .
            catMaybes <$>
            mapM
                (\case
                    (Fn _ _ ident decl _ _ _ _ _ _) -> 
                        Just . (createFunRef ident, ) <$> extractFnType decl
                    _ -> return Nothing)
                items

        createFunRef :: Ident -> FunRef
        createFunRef = 
            flip FunRef Nothing . 
            QualifiedBinding (filePathToNsRef srcFile) .
            toBinding
        
        extractFnType :: CompM m => FnDecl Span -> m FnType
        extractFnType f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        extractFnType (FnDecl args retTyp _ _) = do
            args' <- mapM convertArg args
            return $ FnType args' retTyp
        
        convertArg :: CompM m => Arg Span -> m ArgType
        convertArg (Arg _ typ _) = return $ Normal typ
        convertArg (SelfValue mut _) = undefined
        convertArg a@SelfRegion{} = throwError $ "Self arguments by reference are currently not supported." <> show a
        convertArg (SelfExplicit ty mut _) = return $ Self ty mut