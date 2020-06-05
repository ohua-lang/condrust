module Ohua.Integration.Rust where

import Ohua.Prelude

import Ohua.Frontend.Convert as FC
import Ohua.Backend.Convert as BC
import Ohua.Frontend.Lang as FrLang
import Ohua.Compile.Types

import Language.Rust.Syntax hiding (Rust)
import Language.Rust.Data.Ident
import Language.Rust.Parser ( parse' , Span )
import Language.Rust.Data.InputStream
import Language.Rust.Pretty ( pretty' )
import Data.Text.Prettyprint.Doc

import qualified Data.HashMap.Lazy as HM
import System.FilePath ((</>), splitDirectories, dropExtension)


data Rust where
    Rust :: Rust
    Module :: (FilePath, SourceFile Span) -> Rust

toBinding :: Ident -> Binding
toBinding Ident{name=n} = fromString n

instance Integration Rust where
    frontend srcFile _ = do
        mod <- liftIO load
        ns <- extractNs mod
        return (Module (srcFile, mod), ns)
        where
            load :: IO (SourceFile Span)
            load = parse' <$> readInputStream srcFile

            extractNs :: CompM m => SourceFile Span -> m (Namespace FrLang.Expr)
            -- TODO we might need to retrieve this from the file path.
            extractNs (SourceFile Nothing _ _) = throwError "Missing module name!"
            extractNs (SourceFile (Just modName) _ items) = do
                let filePathToNsRef = makeThrow . map fromString . splitDirectories . dropExtension
                imports <- concat . catMaybes <$>
                        mapM 
                            (\case 
                                (Use _ _ t _) -> Just . toList <$> extractImports [] t
                                _ -> return Nothing)
                            items
                algos <- catMaybes <$>
                        mapM 
                            (\case
                                (Fn _ _ ident decl _ _ _ _ block _) -> Just <$> extractAlgo ident decl block
                                _ -> return Nothing)
                            items
                return $ Namespace (filePathToNsRef srcFile) imports algos

            extractImports :: CompM m => [Binding] -> UseTree Span -> m (NonEmpty Import)
            extractImports prefix (UseTreeSimple path (Just alias) _) = 
                (:|[]) . flip Alias (toBinding alias) . makeThrow . (prefix <>) <$> toBindings path
            extractImports prefix u@(UseTreeSimple _ Nothing _) = 
                throwError $ "Empty 'use' detected. Impossible: This program certainly does not pass 'rustc'." <> show u
            extractImports prefix (UseTreeSimple path Nothing _) = do
                (x:xs) <- reverse <$> toBindings path
                return (Full (makeThrow $ reverse xs) x :| [])
            extractImports prefix (UseTreeGlob path _) = 
                (:|[]) . Glob . makeThrow . (prefix <>)<$> toBindings path
            extractImports prefix u@(UseTreeNested path nesteds _) = do
                path' <- (prefix <>) <$> toBindings path
                nesteds' <- case nonEmpty nesteds of
                                Just n -> return n
                                Nothing -> throwError $ "Empty nested 'use' detected. Impossible: This program certainly does not pass 'rustc'." <> show u
                join <$> mapM (extractImports path') nesteds'

            extractAlgo :: CompM m => Ident -> FnDecl Span -> Block Span -> m (Algo FrLang.Expr)
            extractAlgo Ident{name=n} (FnDecl args _ _ _) block = do
                args' <- mapM FC.convertPat args
                block' <- FC.convertExpr block
                return $ Algo
                            (fromString n) 
                            $ LamE args' block'
            
            toBindings p@(Path _ segments _) =
                forM segments $ \case
                    (PathSegment ident Nothing _) -> return $ toBinding ident
                    (PathSegment _ (Just _) _) -> throwError $ "We currently do not support import paths with path parameters.\n" <> show p
                    

    -- | This is a single file backend.
    -- FIXME The type class does not define the dependency properly via its type.
    backend _ Rust = error "This is simply a weakness in the interface! It should not be possible to call this function without calling 'frontend'. This is always a bug. Please report."
    backend algos (Module (path, SourceFile modName atts items)) =
        let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) algos
            src    = SourceFile modName atts $ map (replaceAlgo algos') items
            render = encodeUtf8 . renderLazy . layoutSmart defaultLayoutOptions . pretty'
        in [render $ src <> "\n"]
        where
            replaceAlgo algos = \case
                    f@(Fn atts vis ident decl s c abi gen _ span) ->
                        case HM.lookup (toBinding ident) algos of
                            Just algo -> 
                                Fn atts vis ident decl s c abi gen (BC.convertExpr algo) span
                            Nothing -> f
                    i -> i

