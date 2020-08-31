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
import qualified Data.HashMap.Lazy as HM


instance Integration Module where
    type Code Module = Block ()

    lower (Module (path, SourceFile _ _ items)) ns = 
        return $ 
            ns & algos %~ map (\algo -> algo & algoCode %~ convertTasks (algo^.algoName))
        where
            convertTasks algo (TCProgram chans retChan tasks) = 
                let algosAndArgs = HM.fromList $ 
                        map (\(Fn _ _ ident (FnDecl args _ _ _) _ _ _ _ _ _) -> 
                                (toBinding ident, args))
                            items
                    args = case HM.lookup algo algosAndArgs of
                                -- TODO: I have currently no idea how to express this invariant in a type.
                            Nothing -> error "Compiler invariant broken: Algo not found in source module."
                            (Just as) -> as
                in TCProgram chans retChan $ map (convertIntoBlock . convertEnvs args)tasks 
                            
            convertEnvs :: [Arg a] -> TCLang.TaskExpr -> TCLang.TaskExpr
            convertEnvs args = cata $ \case
                LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
                e -> embed e

            argToVar :: Rust.Arg a -> TCLang.TaskExpr
            argToVar (Arg (Just (IdentP _ i _ _ )) _ _) = Var $ toBinding i

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
