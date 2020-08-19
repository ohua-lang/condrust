{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types
import Ohua.Backend.Convert

import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Data.Ident
import Language.Rust.Pretty ( pretty' )
import Language.Rust.Quote
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text (unpack)
import Data.List ((!!))
import qualified Data.HashMap.Lazy as HM
import Data.Functor.Foldable (cata, embed)
import System.FilePath (takeFileName)


instance Integration RustLang where
    -- | This is a single file backend.
    -- FIXME The type class does not define the dependency properly via its type.
    backend _ Empty = throwError "This is simply a weakness in the interface! It should not be possible to call this function without calling 'frontend'. This is always a bug. Please report."
    backend ns (Module (path, SourceFile modName atts items)) =
        let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) $ ns^.algos
            src    = SourceFile modName atts $ map (replaceAlgo algos') items
            render = encodeUtf8 . (<> "\n") . renderLazy . layoutSmart defaultLayoutOptions . pretty'
            path' = takeFileName path -- TODO verify this!
        in return $ (path', render src) :| []
        where
            replaceAlgo algos = \case
                    f@(Fn atts vis ident decl@(FnDecl args _ _ _) s c abi gen _ span) ->
                        case HM.lookup (toBinding ident) algos of
                            Just algo -> 
                                Fn atts vis ident decl s c abi gen (span <$ convert args algo) span
                            Nothing -> f
                    i -> i
            
            convert :: [Arg a] -> TCLang.TCExpr -> Block ()
            convert args = convertExpr . convertEnvs args

            convertEnvs :: [Arg a] -> TCLang.TCExpr -> TCLang.TCExpr
            convertEnvs args = cata $ \case
                LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
                e -> embed e

            argToVar :: Rust.Arg a -> TCLang.TCExpr
            argToVar (Arg (Just (IdentP _ i _ _ )) _ _) = Var $ toBinding i 

noSpan = ()

instance ConvertInto (Block ()) where
    convertExpr expr = 
        let expr' = convertExpr expr :: (Expr ())
        in case expr' of
            BlockExpr _ block _ -> block
            _ -> error $ "Expression is not a block!\n " <> show expr

instance ConvertInto (Expr ()) where 
    convertExpr (Var bnd) = 
        PathExpr [] Nothing (convertVar bnd) noSpan

    convertExpr (TCLang.Lit (NumericLit i)) = Rust.Lit [] (Int Dec i Unsuffixed noSpan) noSpan
    convertExpr (TCLang.Lit UnitLit) = TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
    convertExpr (TCLang.Lit (EnvRefLit hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    convertExpr (TCLang.Lit (FunRefLit (FunRef qBnd _))) = PathExpr [] Nothing (convertQualBnd qBnd) noSpan

    convertExpr (Apply (Stateless bnd args)) =
        Call 
            []
            (convertExpr $ TCLang.Lit $ FunRefLit $ FunRef bnd Nothing)
            (map convertExpr args)
            noSpan
    convertExpr (Apply (Stateful var (QualifiedBinding _ bnd) args)) =
        MethodCall
            []
            (convertExpr $ Var var)
            (mkIdent $ unpack $ unwrap bnd)
            Nothing
            (map convertExpr args)
            noSpan

    convertExpr (Lambda args expr) = 
        Closure
            []
            Movable
            Value
            (FnDecl 
                (map 
                    (\bnd -> 
                        Arg 
                            (Just $ mkSimpleBinding $ unwrap bnd)
                            (Infer noSpan)
                            noSpan)
                    args)
                (Just $ Infer noSpan)
                False
                noSpan)
            (convertExpr expr)
            noSpan

    convertExpr (Let bnd stmt cont) = 
        let stmtExpr = Local 
                        (mkSimpleBinding $ unwrap bnd)
                        Nothing
                        (Just $ convertExpr stmt)
                        []
                        noSpan
            contExpr = convertExpr cont
        in append [stmtExpr] contExpr
    
    convertExpr (TCLang.Loop expr) =
        let block = case convertExpr expr of
                        BlockExpr _ b _ -> b
                        e -> Block [Semi e noSpan] Normal noSpan
        in Rust.Loop [] block Nothing noSpan

    convertExpr (Channel numCopies) =
        convertExpr $
            Apply $ 
                Stateless 
                    (QualifiedBinding (makeThrow ["ohua", "arcs", "Channel"]) "new") 
                    [TCLang.Lit $ NumericLit $ fromIntegral numCopies]
    convertExpr (Receive rcvIdx channel) =
        convertExpr $
            Apply $ 
                Stateful 
                    channel 
                    (QualifiedBinding (makeThrow []) "recv") 
                    [TCLang.Lit $ NumericLit $ fromIntegral rcvIdx]
    convertExpr (Send channel d) =
        convertExpr $
            Apply $ 
                Stateful 
                    channel 
                    (QualifiedBinding (makeThrow []) "send")
                    [Var d]
    convertExpr (Run tasks cont) = 
        let taskInitStmt = noSpan <$ [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
            task (Task expr) =
                Apply $
                    Stateless
                        (QualifiedBinding (makeThrow ["Box"]) "new") 
                        [Lambda [] expr]
            push t =
                Apply $
                    Stateful
                        "tasks"
                        (QualifiedBinding (makeThrow []) "push")
                        [t]
            taskStmts = map (flip Semi noSpan . convertExpr . push . task) tasks
            contStmt = convertExpr cont
        in flip append contStmt $ taskInitStmt : taskStmts

mkSimpleBinding bnd = 
    IdentP 
        (ByValue Immutable)
        (mkIdent $ unpack bnd)
        Nothing
        noSpan

convertQualBnd (QualifiedBinding ns bnd) = 
    Path 
        False
        (map 
            (\p -> PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
            $ unwrap ns ++ [bnd])
        noSpan

convertVar bnd = Path False [PathSegment (mkIdent $ unpack $ unwrap bnd) Nothing noSpan] noSpan

append stmts cont = 
    case cont of
        BlockExpr atts (Block contStmts safety s0) s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Normal noSpan) noSpan