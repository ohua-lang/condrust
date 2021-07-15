module Ohua.Integration.Rust.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.TypeExtraction as TE (RustTypeAnno, RustArgType( Normal ))

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Data.Text (unpack)
import Data.List ((!!))
import Data.Functor.Foldable (cata, embed)
import Data.Maybe
import qualified Data.HashMap.Lazy as HM


noSpan :: ()
noSpan = ()

convertIntoBlock :: (Architecture arch, Lang arch ~ (Language 'Rust)) => arch -> TaskExpr RustTypeAnno -> Block ()
convertIntoBlock arch expr = 
    let expr' = convertExpr arch expr
    in case expr' of
        BlockExpr _ block _ _ -> block
        e -> Block [NoSemi e noSpan] Rust.Normal noSpan

instance Integration (Language 'Rust) where
    type NS (Language 'Rust) = Module
    type Type (Language 'Rust) = RustTypeAnno
    type AlgoSrc (Language 'Rust) = Item Span

    type Expr (Language 'Rust) = Rust.Expr ()
    type Task (Language 'Rust) = Rust.Block ()

    lower (Module _path (SourceFile _ _ items)) arch ns = 
        return $ 
            ns & algos %~ map (\algo -> algo & algoCode %~ convertTasks (algo^.algoAnno))
        where
            convertTasks (Fn _ _ _ (FnDecl args typ _ span) _ _ block _) (Program chans (SRecv _ c) tasks) = 
                Program 
                    chans 
                    (SRecv (Type $ TE.Normal $ fromMaybe (TupTy [] span) typ) c)
                    $ map (convertIntoBlock arch . convertEnvs args <$>) tasks 
                            
            convertEnvs :: [Arg a] -> TCLang.TaskExpr RustTypeAnno -> TCLang.TaskExpr RustTypeAnno
            convertEnvs args = cata $ \case
                LitF (EnvRefLit h) -> argToVar (args !! unwrap h)
                e -> embed e

            argToVar :: Rust.Arg a -> TCLang.TaskExpr RustTypeAnno
            argToVar (Arg _ (Just (IdentP _ i _ _ )) _ _) = Var $ toBinding i

    convertExpr _ (Var bnd) = PathExpr [] Nothing (convertVar bnd) noSpan

    convertExpr _ (TCLang.Lit (NumericLit i)) = Rust.Lit [] (Int Dec i Unsuffixed noSpan) noSpan
    convertExpr _ (TCLang.Lit (BoolLit b)) = Rust.Lit [] (Bool b Unsuffixed noSpan) noSpan
    convertExpr _ (TCLang.Lit UnitLit) = TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
    convertExpr _ (TCLang.Lit (EnvRefLit _hostExpr)) = error "Host expression encountered! This is a compiler error. Please report!"
    convertExpr _ (TCLang.Lit (FunRefLit (FunRef qBnd _ _type))) = PathExpr [] Nothing (convertQualBnd qBnd) noSpan

    convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
    convertExpr arch (Apply (Stateful stateExpr (QualifiedBinding _ bnd) args)) =
        MethodCall
            []
            (convertExpr arch stateExpr)
            (PathSegment (mkIdent $ unpack $ unwrap bnd) Nothing noSpan)
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

    convertExpr arch (ReceiveData recv) = convertRecv arch recv
    convertExpr arch (SendData send) = convertSend arch send

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
            range = Rust.Range [] (Just $ convertExpr arch $ TCLang.Lit $ NumericLit 0) (Just repetition) HalfOpen noSpan
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
        convertExpr arch $ Apply $ Stateful (Var bnd) (mkFunRefUnqual "push") [expr]
    convertExpr arch (TCLang.Size bnd) =         
        convertExpr arch $ Apply $ Stateful (Var bnd) (mkFunRefUnqual "len") []

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
    
    convertExpr arch (TCLang.HasSize bnd) =
        let intermediate = toBinding "tmp_has_size"
        in convertExpr arch $ Let intermediate (Apply $ Stateful (Apply $ Stateful (Var bnd) (mkFunRefUnqual "iter") []) (mkFunRefUnqual "size_hint") []) (Apply $ Stateful (Second intermediate) (mkFunRefUnqual "is_some") [])
        -- In the old runtime this was implemented using the `size_hint` function in Rust
        -- What I want here:
        --      v.iter().size_hint().1.is_some()

pattern UnqualFun :: Binding -> QualifiedBinding
pattern UnqualFun bnd <- QualifiedBinding [] bnd

mkFunRefUnqual :: Binding -> QualifiedBinding
mkFunRefUnqual = QualifiedBinding (makeThrow [])

mkSimpleBinding :: Binding -> Pat ()
mkSimpleBinding bnd = 
    IdentP 
        (ByValue Immutable)
        (mkIdent $ unpack $ unwrap bnd)
        Nothing
        noSpan

-- TODO we probably want a Literal for common operations
convertFunCall :: (Architecture arch, Lang arch ~ (Language 'Rust), ty ~ B.Type (Lang arch))
               => arch -> QualifiedBinding -> [TCLang.TaskExpr RustTypeAnno] -> Rust.Expr ()
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
        (convertExpr arch $ TCLang.Lit $ FunRefLit $ FunRef f Nothing Untyped)
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
        BlockExpr atts (Block contStmts safety s0) Nothing s1 -> 
            BlockExpr atts (Block (stmts ++ contStmts) safety s0) Nothing s1
        _ -> BlockExpr [] (Block (stmts ++ [NoSemi cont noSpan]) Rust.Normal noSpan) Nothing noSpan
