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
import qualified Data.HashMap.Lazy as HM


instance Integration RustLang where
    -- | This is a single file backend.
    -- FIXME The type class does not define the dependency properly via its type.
    backend _ Rust = throwError "This is simply a weakness in the interface! It should not be possible to call this function without calling 'frontend'. This is always a bug. Please report."
    backend ns (Module (path, SourceFile modName atts items)) =
        let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) $ ns^.algos
            src    = SourceFile modName atts $ map (replaceAlgo algos') items
            render = encodeUtf8 . (<> "\n") . renderLazy . layoutSmart defaultLayoutOptions . pretty'
            path' = path -- TODO verify this!
        in return $ (path', render src) :| []
        where
            replaceAlgo algos = \case
                    f@(Fn atts vis ident decl s c abi gen _ span) ->
                        case HM.lookup (toBinding ident) algos of
                            Just algo -> 
                                Fn atts vis ident decl s c abi gen (span <$ (convertExpr algo :: Block ())) span
                            Nothing -> f
                    i -> i

noSpan = ()

instance ConvertInto (Block ()) where
    convertExpr expr = 
        let expr' = convertExpr expr :: (Expr ())
        in case expr' of
            BlockExpr _ block _ -> block
            _ -> error $ "Expression is not a block!\n " <> show expr

instance ConvertInto (Expr ()) where 
    convertExpr (Binding v) = 
        PathExpr [] Nothing (convertVar v) noSpan

    convertExpr (TCLang.Lit (NumericLit i)) = Rust.Lit [] (Int Dec i Unsuffixed noSpan) noSpan
    convertExpr (TCLang.Lit UnitLit) = TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
    convertExpr (TCLang.Lit (EnvRefLit hostExpr)) = error "Host expression encountered!"
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
            (convertExpr $ Binding var)
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
                    [Binding d]
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
                        (Var "tasks")
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
        (not $ null $ unwrap ns)
        (map 
            (\p -> PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
            $ unwrap ns ++ [bnd])
        noSpan

convertVar (Var v) = Path False [PathSegment (mkIdent $ unpack v) Nothing noSpan] noSpan

append stmts cont = 
    case cont of
        BlockExpr atts (Block contStmts safety s0) s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Normal noSpan) noSpan