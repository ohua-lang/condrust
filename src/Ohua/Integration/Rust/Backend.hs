module Ohua.Integration.Rust.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types
import Ohua.Backend.Convert

import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Data.Ident
import Data.Text (unpack)
import Data.List ((!!))
import Data.Functor.Foldable (cata, embed)


instance Integration RustLang where
    type Code RustLang = Block ()

    lower Empty _ = throwError "This is simply a weakness in the interface! It should not be possible to call this function without calling 'frontend'. This is always a bug. Please report."
    lower (Module (path, SourceFile _ _ items)) ns =  
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ convertTasks)
        where
            convertTasks (TCProgram chans retChan tasks) = 
                TCProgram chans retChan $ map convertIntoBlock tasks 
            
            -- FIXME The support for EnvRefs is in fact architecture-dependent.
            --       For the microservice case, there are no EnvRefs.
            --       I still need to find a way to attach this code to the architecture type class.
            -- SOLUTION EnvRefs should just be of type Binding. They refer to an idx only for
            --          historic reasons. 

            -- convertEnvs :: [Arg a] -> TCLang.TaskExpr -> TCLang.TaskExpr
            -- convertEnvs args = cata $ \case
            --     LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
            --     e -> embed e

            -- argToVar :: Rust.Arg a -> TCLang.TaskExpr
            -- argToVar (Arg (Just (IdentP _ i _ _ )) _ _) = Var $ toBinding i

noSpan = ()

convertIntoBlock :: TaskExpr -> Block ()
convertIntoBlock expr = 
    let expr' = convertExpr expr :: (Expr ())
    in case expr' of
        BlockExpr _ block _ -> block
        e -> Block [NoSemi e noSpan] Normal noSpan

instance ConvertExpr (Expr ()) where
    convertExpr (Var bnd) = PathExpr [] Nothing (convertVar bnd) noSpan

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
                        (mkSimpleBinding bnd)
                        Nothing
                        (Just $ convertExpr stmt)
                        []
                        noSpan
            contExpr = convertExpr cont
        in prependToBlock [stmtExpr] contExpr

    convertExpr (TCLang.Stmt stmt cont) = 
        prependToBlock [Semi (convertExpr stmt) noSpan] $ convertExpr cont

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
        let block = convertIntoBlock expr
        in Rust.Loop [] block Nothing noSpan

    convertExpr (TCLang.Loop bnd colBnd expr) = 
        let block = convertIntoBlock expr
            colExpr = convertExpr $ Var colBnd
            loopVar = mkSimpleBinding bnd
        in Rust.ForLoop [] loopVar colExpr block Nothing noSpan


mkSimpleBinding :: Binding -> Pat ()
mkSimpleBinding bnd = 
    IdentP 
        (ByValue Immutable)
        (mkIdent $ unpack $ unwrap bnd)
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

prependToBlock :: [Rust.Stmt ()] -> Expr () -> Expr ()
prependToBlock stmts cont = 
    case cont of
        BlockExpr atts (Block contStmts safety s0) s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Normal noSpan) noSpan
