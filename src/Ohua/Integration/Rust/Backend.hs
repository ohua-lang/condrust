module Ohua.Integration.Rust.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

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


noSpan = ()

convertIntoBlock :: (Architecture arch, Lang arch ~ Module) => arch -> TaskExpr -> Block ()
convertIntoBlock arch expr = 
    let expr' = convertExpr arch expr
    in case expr' of
        BlockExpr _ block _ -> block
        e -> Block [NoSemi e noSpan] Normal noSpan

instance Integration Module where
    type RetChan Module = TaskExpr
    type Expr Module = Rust.Expr ()
    type Task Module = Block ()

    lower (Module (path, SourceFile _ _ items)) arch ns = 
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
                            -- FIXME: The proper thing to do would be not to look it up but to tie the algo 
                            --        to what we are compiling!
                            Nothing -> error "Compiler invariant broken: Algo not found in source module."
                            (Just as) -> as
                in TCProgram chans retChan $ map (convertIntoBlock arch . convertEnvs args) tasks 
                            
            convertEnvs :: [Arg a] -> TCLang.TaskExpr -> TCLang.TaskExpr
            convertEnvs args = cata $ \case
                LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
                e -> embed e

            argToVar :: Rust.Arg a -> TCLang.TaskExpr
            argToVar (Arg (Just (IdentP _ i _ _ )) _ _) = Var $ toBinding i

-- instance ConvertTaskExpr (Expr ()) where
    convertExpr _ (Var bnd) = PathExpr [] Nothing (convertVar bnd) noSpan

    convertExpr _ (TCLang.Lit (NumericLit i)) = Rust.Lit [] (Int Dec i Unsuffixed noSpan) noSpan
    convertExpr _ (TCLang.Lit (BoolLit b)) = Rust.Lit [] (Bool b Unsuffixed noSpan) noSpan
    convertExpr _ (TCLang.Lit UnitLit) = TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
    convertExpr _ (TCLang.Lit (EnvRefLit hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd _))) = PathExpr [] Nothing (convertQualBnd qBnd) noSpan

    convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
    convertExpr arch (Apply (Stateful var (QualifiedBinding _ bnd) args)) =
        MethodCall
            []
            (convertExpr arch $ Var var)
            (mkIdent $ unpack $ unwrap bnd)
            Nothing
            (map (convertExpr arch) args)
            noSpan

    convertExpr arch (Let bnd stmt cont) = 
        let stmtExpr = Local 
                        (mkSimpleBinding bnd)
                        Nothing
                        (Just $ convertExpr arch stmt)
                        []
                        noSpan
            contExpr = convertExpr arch cont
        in prependToBlock [stmtExpr] contExpr

    convertExpr arch (TCLang.Stmt stmt cont) = 
        prependToBlock [Semi (convertExpr arch stmt) noSpan] $ convertExpr arch cont

    convertExpr arch (TCLang.Assign bnd expr) =
        prependToBlock 
            [Semi (Rust.Assign [] (convertExpr arch $ Var bnd) (convertExpr arch expr) noSpan) noSpan]
            $ convertExpr arch $ TCLang.Lit UnitLit

    convertExpr arch (ReceiveData recv) = convertExpr arch $ convertRecv arch recv
    convertExpr arch (SendData send) = convertExpr arch $ convertSend arch send

    convertExpr arch (TCLang.EndlessLoop expr) =
        let block = convertIntoBlock arch expr
        in Rust.Loop [] block Nothing noSpan

    convertExpr arch (TCLang.ForEach bnd colBnd expr) = 
        let block = convertIntoBlock arch expr
            colExpr = convertExpr arch $ Var colBnd
            loopVar = mkSimpleBinding bnd
        in Rust.ForLoop [] loopVar colExpr block Nothing noSpan

    convertExpr arch (TCLang.Repeat count expr) = 
        let loopVar = mkSimpleBinding "_"
            repetition = case count of
                            Left bnd -> convertExpr arch $ Var bnd
                            Right cnt -> convertExpr arch (TCLang.Lit $ NumericLit $ fromIntegral cnt)
            range = Rust.Repeat [] (convertExpr arch $ TCLang.Lit $ NumericLit 0) repetition noSpan
            block = convertIntoBlock arch expr
        in Rust.ForLoop [] loopVar range block Nothing noSpan

    convertExpr arch (TCLang.While loopHead body) =
        Rust.While [] (convertExpr arch loopHead) (convertIntoBlock arch body) Nothing noSpan

    convertExpr arch (TCLang.Cond condExpr trueBranch falseBranch) =
        Rust.If [] 
            (convertExpr arch condExpr) 
            (convertIntoBlock arch trueBranch)
            (Just $ convertExpr arch falseBranch)
            noSpan

    convertExpr arch (TCLang.ListOp Create) = convertExpr arch $ Apply $ Stateless "Vec/new" []
    convertExpr arch (TCLang.ListOp (Append bnd expr)) = 
        convertExpr arch $ Apply $ Stateful bnd (mkFunRefUnqual "push") [expr]
    convertExpr arch (TCLang.Size bnd) =         
        convertExpr arch $
            Apply $ 
                Stateful 
                    bnd 
                    (mkFunRefUnqual "len")
                    []

    convertExpr arch (TCLang.Tuple one two) = 
        let conv = convertExpr arch . either TCLang.Var TCLang.Lit
        in Rust.TupExpr [] [conv one, conv two] noSpan    
    convertExpr arch (TCLang.First bnd) = Rust.TupField [] (convertExpr arch $ Var bnd) 0 noSpan
    convertExpr arch (TCLang.Second bnd) = Rust.TupField [] (convertExpr arch $ Var bnd) 1 noSpan

    convertExpr arch (TCLang.Increment bnd) = 
        convertExpr arch $
        TCLang.Assign bnd $ Apply $ Stateless (mkFunRefUnqual "+") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr arch (TCLang.Decrement bnd) = 
        convertExpr arch $
        TCLang.Assign bnd $ Apply $ Stateless (mkFunRefUnqual "-") [Var bnd, TCLang.Lit $ NumericLit 1]
    convertExpr arch (TCLang.Not expr) = convertExpr arch $ Apply $ Stateless (mkFunRefUnqual "!") [expr]
    
    convertExpr _ (TCLang.HasSize bnd) = undefined -- TODO This is really something that we need to reconsider/redesign!
    convertExpr _ (TCLang.Generate bnd lit) = undefined -- TODO 

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
convertFunCall :: (Architecture arch, Lang arch ~ Module) => arch -> QualifiedBinding -> [TCLang.TaskExpr] -> Rust.Expr ()
convertFunCall arch op [arg1, arg2] | isJust $ binOp op = 
    Binary [] (fromJust $ binOp op) (convertExpr arch arg1) (convertExpr arch arg2) noSpan
    where
        binOp = \case
            UnqualFun "+" -> Just Rust.AddOp
            UnqualFun "-" -> Just Rust.SubOp
            UnqualFun "*" -> Just Rust.MulOp
            UnqualFun "/" -> Just Rust.DivOp
            _ -> Nothing
convertFunCall arch op [arg] | isJust $ unOp op = 
    Unary [] (fromJust $ unOp op) (convertExpr arch arg) noSpan 
    where
        unOp = \case
            UnqualFun "!" -> Just Rust.Not
            UnqualFun "-" -> Just Rust.Neg
            UnqualFun "*" -> Just Rust.Deref
            _ -> Nothing
convertFunCall arch f args = 
    Call 
        []
        (convertExpr arch $ TCLang.Lit $ FunRefLit $ FunRef f Nothing)
        (map (convertExpr arch) args)
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

prependToBlock :: [Rust.Stmt ()] -> Rust.Expr () -> Rust.Expr ()
prependToBlock stmts cont = 
    case cont of
        BlockExpr atts (Block contStmts safety s0) s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Normal noSpan) noSpan
