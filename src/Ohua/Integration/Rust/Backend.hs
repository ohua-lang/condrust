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
    type Code RustLang = Block ()

    convert _ Empty = throwError "This is simply a weakness in the interface! It should not be possible to call this function without calling 'frontend'. This is always a bug. Please report."
    convert ns (Module (path, SourceFile _ _ items)) =  
        return ns & algos %~ map (\algo -> algo & algoCode %~ convertTasks)
        where
            convertTasks (TCProgram chans retChan tasks) = 
                TCProgram chans retChan $ map convertExpr tasks 
            
            -- FIXME The support for EnvRefs is in fact architecture-dependent.
            --       For the microservice case, there are no EnvRefs.
            --       I still need to find a way to attach this code to the architecture type class.

            -- convertEnvs :: [Arg a] -> TCLang.TaskExpr -> TCLang.TaskExpr
            -- convertEnvs args = cata $ \case
            --     LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
            --     e -> embed e

            -- argToVar :: Rust.Arg a -> TCLang.TaskExpr
            -- argToVar (Arg (Just (IdentP _ i _ _ )) _ _) = Var $ toBinding i

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

    convertExpr (Let bnd stmt cont) = 
        let stmtExpr = Local 
                        (mkSimpleBinding $ unwrap bnd)
                        Nothing
                        (Just $ convertExpr stmt)
                        []
                        noSpan
            contExpr = convertExpr cont
        in appendToBlock [stmtExpr] contExpr

    convertExpr (TCLang.Stmt stmt cont) = 
        appentToBlock [Semi (convertExpr stmt) noSpan] $ convertExpr cont
    
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

    convertExpr (TCLang.EndlessLoop expr) =
        let block = case convertExpr expr of
                        BlockExpr _ b _ -> b
                        e -> Block [Semi e noSpan] Normal noSpan
        in Rust.Loop [] block Nothing noSpan

    convertExpr (TCLang.Loop bnd colBnd expr) = undefined -- TODO

mkSimpleBinding :: Binding -> Pat ()
mkSimpleBinding bnd = 
    IdentP 
        (ByValue Immutable)
        (mkIdent $ unpack bnd)
        Nothing
        noSpan

convertQualBnd :: QualifiedBinding -> Path ()
convertQualBnd (QualifiedBinding ns bnd) = 
    Path 
        False
        (map 
            (\p -> PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
            $ unwrap ns ++ [bnd])
        noSpan

convertVar :: Binding -> Path ()
convertVar bnd = Path False [PathSegment (mkIdent $ unpack $ unwrap bnd) Nothing noSpan] noSpan

appendToBlock :: [Rust.Stmt ()] -> Expr () -> Expr ()
appendToBlock stmts cont = 
    case cont of
        BlockExpr atts (Block contStmts safety s0) s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Normal noSpan) noSpan