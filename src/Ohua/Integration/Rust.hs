module Ohua.Integration.Rust where

import Ohua.Prelude

import Ohua.Frontend.Convert as FC
import Ohua.Backend.Convert as BC
import Ohua.Frontend.Lang as FrLang

import Language.Rust.Syntax
import Language.Rust.Parser ( parse' )
import Language.Rust.Data.InputStream
import Language.Rust.Pretty ( pretty' )
import Data.Text.Prettyprint.Doc


newtype RustCtxt exp pat = RustCtxt SourceFile

instance Integration (RustCtxt (Block a) (Pat a)) where
    frontend file = do
        mod <- liftIO load
        ns <- extractNs mod
        return (ns, RustCtxt mod)
        where
            load :: IO (SourceFile a)
            load = parse' $ readInputStream file

            extractNs :: CompM m => SourceFile a -> m Namespace FrLang.Expr
            -- TODO we might need to retrieve this from the file path.
            extractNs (SourceFile Nothing _ _) = throwError "Missing module name!"
            extractNs (SourceFile (Just modName) _ items) = do
                imports <- concat . catMaybes <$>
                        forM items $ \case 
                            (Use _ _ t _) -> Just . toList <$> extractImports [] t
                            _ -> return Nothing
                algos <- catMaybes <$>
                        forM items $ \case
                            (Fn _ _ ident decl _ _ _ _ block _) -> Just <$> extractAlgo ident decl block
                            _ -> return Nothing
                return $ Namespace modName imports algos

            extractImports :: CompM m  => [Binding] -> UseTree a -> m (NonEmpty Import)
            extractImports prefix (UseTreeSimple path (Just alias) _) = (:|[]) . flip Alias (toBinding alias) . make . (prefix <>) <$> toBindings path
            extractImports prefix u@(UseTreeSimple [] Nothing _) = throwError $ "Empty 'use' detected. Impossible: This program certainly does not pass 'rustc'." <> show u
            extractImports prefix (UseTreeSimple path Nothing _) = do
                (x:xs) <- reverse <$> toBindings path
                return (Full (make $ reverse xs) x :| [])
            extractImports prefix (UseTreeGlob path _) = (:|[]) . Glob . make . (prefix <>)<$> toBindings path
            extractImports prefix (UseTreeNested path nesteds _) = do
                path' <- (prefix <>) <$> toBindings path
                concat <$> forM nesteds $ extractImports path'

            extractAlgo :: CompM m => Ident -> FnDecl a -> Block a -> m Algo
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
                    
            toBinding Ident{name=n} = fromString n

    -- | This is a single file backend.
    backend (RustCtxt (SourceFile modName atts items)) algos =
        let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) algos
            src    = SourceFile modName atts $ map replaceAlgo items
            render = encodeUtf8 . renderLazy . layoutSmart defaultLayoutOptions . pretty'
        in [render $ src <> "\n"]
        where
            replaceAlgo (SourceFile modName atts items) algo = 
                SourceFile modName atts $ flip map items $ \case
                    (Fn atts vis ident decl s c abi gen block a) ->
                        let block' = BC.convertExpr block
                        in Fn atts vis ident decl s c abi gen block' a
                    i -> i

