{-# LANGUAGE QuasiQuotes #-}

module Ohua.Backend.Convert.Rust where

import Ohua.Prelude

import Ohua.Backend.Convert
import Ohua.Backend.TCLang as TCLang
import Language.Rust.Syntax as Rust
import Language.Rust.Quote
import Language.Rust.Data.Ident

import Data.Text (unpack)

noSpan = ()

instance ConvertInto (Expr a) where 
    convertExpr (Binding v) = 
        PathExpr [] Nothing (convertVar v) noSpan

    convertExpr (TCLang.Lit (NumericLit i)) = Rust.Lit $ Int Dec i Unsuffixed
    convertExpr (TCLang.Lit UnitLit) = TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
    convertExpr (TCLang.Lit (EnvRefLit hostExpr)) = error "Host expression encountered!"
    convertExpr (TCLang.Lit (FunRef qBnd _)) = PathExpr [] Nothing (convertQualBnd qBnd) noSpan

    convertExpr (Apply (Stateless bnd args)) =
        Call 
            []
            (convertExpr $ TCLang.Lit $ FunRef bnd Nothing)
            (convertArgs args)
            noSpan
    convertExpr (Apply (Stateful var (QualifiedBinding _ bnd) args)) =
        MethodCall
            []
            (convertVar $ Binding var)
            (mkIdent $ unwrap bnd)
            Nothing
            (convertArgs args)
            noSpan

    convertExpr (Lambda args expr) = 
        Closure
            []
            Movable
            Value
            (FnDecl 
                (map 
                    (\(Var bnd) -> 
                        Arg 
                            (Just $ mkSimpleBinding bnd)
                            (Infer noSpan)
                            noSpan)
                    args)
                (Just $ Infer noSpan)
                False
                noSpan)
            (convertExpr expr)
            noSpan

    convertExpr (Let (Var bnd) stmt cont) = 
        let stmtExpr = Local 
                        (mkSimpleBinding bnd)
                        Nothing
                        (Just $ convertExpr stmt)
                        []
                        noSpan
            contExpr = convertExpr cont
        in append [stmtExpr] contExpr
    
    convertExpr (TCLang.Loop expr) =
        let block = case convertExpr expr of
                        e@(BlockExpr _ _ _) -> e
                        e -> BlockExpr [] (Block [Semi e noSpan] Normal noSpan) noSpan
        in Rust.Loop [] block Nothing noSpan

    convertExpr (Channel numCopies) =
        convertExpr $
            Apply $ 
                Stateless 
                    (QualifiedBinding (makeThrow ["ohua", "arcs", "Channel"]) "new") 
                    [TCLang.Lit $ NumericLit numCopies]
    convertExpr (Receive rcvIdx channel) =
        convertExpr $
            Apply $ 
                Stateful 
                    channel 
                    (QualifiedBinding (makeThrow []) "recv") 
                    [Right $ TCLang.Lit $ NumericLit rcvIdx]
    convertExpr (Send channel d) =
        convertExpr $
            Apply $ 
                Stateful 
                    channel 
                    (QualifiedBinding (makeThrow []) "send")
                    [Left d]
    convertExpr (Run tasks cont) = 
        let taskInitStmt = [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
            task = \(Task expr) -> 
                Apply $
                    Stateless
                        (QualifiedBinding (makeThrow ["Box"]) "new") 
                        [Lambda [] expr]
            push = \t -> 
                Apply $
                    Stateful
                        (Var "tasks")
                        (QualifiedBinding (makeThrow []) "push")
                        [t]
            taskStmts = map (flip Semi noSpan . convertExpr . push . task) tasks
            contStmt = convertExpr cont
        in flip append contStmt $ [taskInitStmt] ++ taskStmts

mkSimpleBinding bnd = 
    IdentP 
        (ByValue Immutable)
        (mkIdent $ unpack bnd)
        (Just $ Infer noSpan)
        noSpan

convertQualBnd (QualifiedBinding ns bnd) = 
    (Path 
        ((length $ unwrap ns) > 0)
        (map 
            (\p -> PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
            $ (unwrap ns) ++ [bnd])
        noSpan)

convertVar (Var v) = Path False [PathSegment (mkIdent $ unpack v) Nothing noSpan] noSpan

convertArgs = map (convertExpr . either Binding id)

append stmts cont = 
    case cont of
        BlockExpr atts (Block contStmts safety s0) s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Normal noSpan) noSpan