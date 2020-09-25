module Ohua.Integration.Rust.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types
import Ohua.Backend.Convert

import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Data.Ident
import Language.Rust.Quote (expr)
import Data.Text (unpack)
import Data.List ((!!))
import Data.Functor.Foldable (cata, embed)
import Data.Maybe
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
    convertExpr (TCLang.Lit (BoolLit b)) = Rust.Lit [] (Bool b Unsuffixed noSpan) noSpan
    convertExpr (TCLang.Lit UnitLit) = TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
    convertExpr (TCLang.Lit (EnvRefLit hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    convertExpr (TCLang.Lit (FunRefLit (FunRef qBnd _))) = PathExpr [] Nothing (convertQualBnd qBnd) noSpan

    convertExpr (Apply (Stateless bnd args)) = convertFunCall bnd args
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

    convertExpr (TCLang.Assign bnd expr) =
        prependToBlock 
            [Semi (Rust.Assign [] (convertExpr $ Var bnd) (convertExpr expr) noSpan) noSpan]
            $ convertExpr $ TCLang.Lit UnitLit

    convertExpr (Receive rcvIdx channel) =
        convertExpr $
            Apply $ 
                Stateful 
                    channel 
                    (mkFunRefUnqual "recv") 
                    [TCLang.Lit $ NumericLit $ fromIntegral rcvIdx]
    convertExpr (Send channel d) =
        convertExpr $
            Apply $ 
                Stateful 
                    channel 
                    (mkFunRefUnqual "send")
                    [Var d]

    convertExpr (TCLang.EndlessLoop expr) =
        let block = convertIntoBlock expr
        in Rust.Loop [] block Nothing noSpan

    convertExpr (TCLang.ForEach bnd colBnd expr) = 
        let block = convertIntoBlock expr
            colExpr = convertExpr $ Var colBnd
            loopVar = mkSimpleBinding bnd
        in Rust.ForLoop [] loopVar colExpr block Nothing noSpan

    convertExpr (TCLang.Repeat count expr) = 
        let loopVar = mkSimpleBinding "_"
            repetition = case count of
                            Left bnd -> convertExpr $ Var bnd
                            Right cnt -> convertExpr (TCLang.Lit $ NumericLit $ fromIntegral cnt)
            range = Rust.Repeat [] (convertExpr $ TCLang.Lit $ NumericLit 0) repetition noSpan
            block = convertIntoBlock expr
        in Rust.ForLoop [] loopVar range block Nothing noSpan

    convertExpr (TCLang.While loopHead body) =
        Rust.While [] (convertExpr loopHead) (convertIntoBlock body) Nothing noSpan

    convertExpr (TCLang.Cond condExpr trueBranch falseBranch) =
        Rust.If [] 
            (convertExpr condExpr) 
            (convertIntoBlock trueBranch)
            (Just $ convertExpr falseBranch)
            noSpan

    convertExpr (TCLang.ListOp Create) = convertExpr $ Apply $ Stateless "Vec/new" []
    convertExpr (TCLang.ListOp (Append bnd expr)) = 
        convertExpr $ Apply $ Stateful bnd (mkFunRefUnqual "push") [expr]
    convertExpr (TCLang.Size bnd) =         
        convertExpr $
            Apply $ 
                Stateful 
                    bnd 
                    (mkFunRefUnqual "len")
                    []

    convertExpr (TCLang.Tuple one two) = 
        let conv = convertExpr . either TCLang.Var TCLang.Lit
        in Rust.TupExpr [] [conv one, conv two] noSpan    
    convertExpr (TCLang.First bnd) = Rust.TupField [] (convertExpr $ Var bnd) 0 noSpan
    convertExpr (TCLang.Second bnd) = Rust.TupField [] (convertExpr $ Var bnd) 1 noSpan

    convertExpr (TCLang.Increment bnd) = 
        convertExpr $
        TCLang.Assign bnd $ Apply $ Stateless (mkFunRefUnqual "+") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr (TCLang.Decrement bnd) = 
        convertExpr $
        TCLang.Assign bnd $ Apply $ Stateless (mkFunRefUnqual "-") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr (TCLang.Not expr) = convertExpr $ Apply $ Stateless (mkFunRefUnqual "!") [expr]
    
    convertExpr (TCLang.HasSize bnd) = undefined -- TODO This is really something that we need to reconsider/redesign!
    convertExpr (TCLang.Generate bnd lit) = undefined -- TODO 

pattern UnqualFun :: Binding -> QualifiedBinding
pattern UnqualFun bnd <- QualifiedBinding [] bnd

mkFunRefUnqual = QualifiedBinding (makeThrow [])

mkSimpleBinding :: Binding -> Pat ()
mkSimpleBinding bnd = 
    IdentP 
        (ByValue Immutable)
        (mkIdent $ unpack $ unwrap bnd)
        Nothing
        noSpan

-- TODO we probably want a Literal for common operations
convertFunCall :: QualifiedBinding -> [TCLang.TaskExpr] -> Expr ()
convertFunCall op [arg1, arg2] | isJust $ binOp op = 
    Binary [] (fromJust $ binOp op) (convertExpr arg1) (convertExpr arg2) noSpan
    where
        binOp = \case
            UnqualFun "+" -> Just Rust.AddOp
            UnqualFun "-" -> Just Rust.SubOp
            UnqualFun "*" -> Just Rust.MulOp
            UnqualFun "/" -> Just Rust.DivOp
            _ -> Nothing
convertFunCall op [arg] | isJust $ unOp op = 
    Unary [] (fromJust $ unOp op) (convertExpr arg) noSpan 
    where
        unOp = \case
            UnqualFun "!" -> Just Rust.Not
            UnqualFun "-" -> Just Rust.Neg
            UnqualFun "*" -> Just Rust.Deref
            _ -> Nothing
convertFunCall f args = 
    Call 
        []
        (convertExpr $ TCLang.Lit $ FunRefLit $ FunRef f Nothing)
        (map convertExpr args)
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
