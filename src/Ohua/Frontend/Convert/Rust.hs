module Ohua.Frontend.Convert.Rust where

import Ohua.Prelude

import Ohua.Frontend.Convert
import Ohua.Frontend.Lang as FrLang
import Language.Rust.Syntax as Rust
import Language.Rust.Data.Ident


instance ConvertExpr (Rust.Expr a) where 
    convertExpr e@Box{} = throwError $ "Currently, we do not support the construction of boxed values. Please do so in a function." <> show e
    convertExpr e@InPlace{} = throwError $ "Currently, we do not support in-place expressions.\n" <> show e
    convertExpr e@Vec{} = throwError $ "Currently, we do not support array expressions. Please do so in a function.\n" <> show e
    convertExpr (Call [] fun args _) = do
        fun' <- convertExpr fun
        args' <- mapM convertExpr args
        return $ fun' `AppE` args'
    convertExpr e@Call{} = throwError $ "Currently, we do not support attributes on function calls.\n" <> show e
    convertExpr (MethodCall [] receiver method Nothing args _) = do
        receiver' <- convertExpr receiver
        let method' = convertExpr method
        args' <- mapM convertExpr args
        return (BindE receiver' method') `AppE` args
    convertExpr e@(MethodCall [] _ _ (Just _) _ _) = throwError $ "Currently, we do not support type parameters for function calls. Your best shot: wrap the call into a function.\n" <> show e
    convertExpr e@MethodCall{} = throwError $ "Currently, we do not support attributes on method calls.\n" <> show e
    convertExpr (TupExp [] vars _) = do
        vars' <- mapM converFrom vars
        return $ TupE vars'
    convertExpr e@TupExpr{} = throwError $ "Currently, we do not support attributes on tuple exressions.\n" <> show e
    convertExpr (Binary [] op left right _) = do
        let op' = convertExpr op
        left' <- convertExpr left
        right' <- convertExpr right
        return $ op' `AppE` [left', right']
    convertExpr e@Binary{} = throwError $ "Currently, we do not support attributes on binary operations.\n" <> show e
    convertExpr (Unary [] op arg _) = do
        let op' = convertExpr op
        arg' <- convertExpr arg
        return $ op' `AppE` [arg']
    convertExpr e@Unary{} = throwError $ "Currently, we do not support attributes on unary operations.\n" <> show e
    convertExpr (Lit [] l _) = convertExpr l
    convertExpr e@Lit{} = throwError $ "Currently, we do not support attributes on unary operations.\n" <> show e
    convertExpr e@Cast{} = throwError $ "Currently, we do not support cast expressions. Please use a function.\n" <> show e
    convertExpr e@TypeAscription{} = throwError $ "Currently, we do not support type ascriptions. Please use a function.\n" <> show e
    convertExpr (If [] expr trueBlock falseBlock _) = do
        expr' <- convertExpr expr
        trueBlock' <- convertExpr trueBlock
        falseBlock' <- maybe (return $ FrLang.Lit UnitLit) convertExpr falseBlock
        return $ IfE expr' trueBlock' falseBlock'
    convertExpr e@If{} = throwError $ "Currently, we do not support attributes on conditional expressions.\n" <> show e
    convertExpr e@IfLet{} = throwError $ "Currently, we do not support if-let expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr (While [] cond block Nothing _) = do
        expr' <- convertExpr expr
        block' <- convertExpr block
        return $ 
            -- FIXME proper name generation needed here!
            LetE (VarE "while_loop_body") (Lambda [] $ StmtE block' $ Lit UnitLit)
                $ IfE expr' 
                    (VarE "while_loop_body" `AppE` [])
                    (Lit UnitLit)
    convertExpr e@(While [] _ _ (Just _) _) = throwError $ "Currently, we do not support loop labels.\n" <> show e
    convertExpr e@While{} = throwError $ "Currently, we do not support attributes on while loops.\n" <> show e
    convertExpr e@WhileLet{} = throwError $ "Currently, we do not support if-let expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr (ForLoop [] pat dataExpr body Nothing _) = do
        pat' <- convertExpr pat
        dataExpr' <- convertExpr dataExpr
        body' <- convertExpr body
        return $
            mapE
                (LambdaE [pat'] body)
                dataExpr'
    convertExpr e@(ForLoop [] _ _ _ (Just _) _) = throwError $ "Currently, we do not support loop labels.\n" <> show e
    convertExpr e@ForLoop{} = throwError "Currently, we do not support attributes on for loops.\n" <> show e
    convertExpr e@Loop{} = throwError $ "Currently, we do not support conditionless loops. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr e@Match{} = throwError $ "Currently, we do not support match expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr (Closure [] Movable Value (FnDecl args _ False _) body _) = do
        -- FIXME We are again dropping the type info here which may later on be needed in the code gen.
        args' <- mapM convertPat args
        body' <- convertExpr body
        return $ LambdaE args' body'
    convertExpr e@(Closure _ _ _ (FnDecl _ _ True _) _ _) = throwError $ "Currently, we do not support variadic argument lists. \n" <> show e
    convertExpr e@(Closure _ Immovable _ _ _ _) = throwError $ "Currently, we do not support immovable closures. \n" <> show e
    convertExpr e@(Closure _ _ Ref _ _ _) = throwError $ "Currently, we do not support closures that capture environment variables by reference. \n" <> show e
    convertExpr e@Closure{} = throwError $ "Currently, we do not support attributes on closures.\n" <> show e
    convertExpr e@(BlockExpr [] block _) = convertExpr block
    convertExpr e@BlockExpr{} = throwError $ "Currently, we do not support attributes on block expressions.\n" <> show e
    convertExpr e@Catch{} = throwError $ "Currently, we do not support catch expressions. Please use a function. \n" <> show e
    convertExpr e@Assign{} = throwError $ "Currently, we do not support assign expressions (because memory is managed inside the functions). Please use a function. \n" <> show e
    convertExpr e@AssignOp{} = throwError $ "Currently, we do not support assign-op expressions (because memory is managed inside the functions). Please use a function. \n" <> show e
    convertExpr e@FieldAccess{} = throwError $ "Currently, we do not support field access expressions (because memory/state is managed inside the functions). Please use a function. \n" <> show e
    convertExpr e@TupField{} = throwError $ "Currently, we do not support tuple field expressions. Please use a function. \n" <> show e
    convertExpr e@Index{} = throwError $ "Currently, we do not support indexing expressions. Please use a function. \n" <> show e
    convertExpr e@Range{} = throwError $ "Currently, we do not support range expressions. Please use a function. \n" <> show e
    convertExpr e@Index{} = throwError $ "Currently, we do not support indexing expressions. Please use a function. \n" <> show e
    convertExpr (PathExpr [] Nothing path _) = convertExpr path
    convertExpr e@(PathExpr [] (Just _) _ _) = throwError $ "Currently, we do not support paths to 'self', i.e., compilation of 'impl' functions. \n" <> show e
    convertExpr e@PathExpr{} = throwError $ "Currently, we do not support attributes on path expressions.\n" <> show e
    convertExpr e@AddrOf{} = throwError $ seqParProgNote <> "\n" <> show e
    convertExpr e@Break{} = throwError $ "Currently, we do not support 'break' expressions. Please reformulate the loop. \n" <> show e
    convertExpr e@Continue{} = throwError $ "Currently, we do not support 'continue' expressions. Please reformulate the loop. \n" <> show e
    convertExpr e@Ret{} = throwError $ "Currently, we do not support 'return' expressions. Please reformulate into an expression without a semicolon. \n" <> show e
    convertExpr e@MacExpr{} = throwError $ "Currently, we do not support macro invocations. \n" <> show e
    convertExpr e@StructExpr{} = throwError $ "Currently, we do not support struct literal expressions. Please use a function.\n" <> show e
    convertExpr e@Repeat{} = throwError $ "Currently, we do not support array construction expressions. Please use a function.\n" <> show e
    convertExpr e@ParenExpr{} = throwError $ "Impossible per documentation of language-rust.\n" <> show e
    convertExpr e@Try{} = throwError $ "Currently, we do not support error handling expressions. Please use a function.\n" <> show e
    convertExpr e@Yield{} = throwError $ "Currently, we do not support generator/yield expressions. Please use a function.\n" <> show e

instance ConvertExpr (Path a) where
    -- This needs context information to distinguish a path from a variable.
    -- A transformation is performing this disambiguation later on.
    convertExpr (Path _ [segment] _) = return $ VarE $ Binding $ convertSegment segment
    convertExpr (Path _ segments _) = do 
        segments' <- mapM convertSegment segments
        let (x:revPath) = reverse segments'
        return $ Lit $ FunRefLit $ FunRef (QualifiedBinding (makeThrow $ reverse revPath) x) Nothing
        where 
            convertSegment (PathSegment Ident{name=n} Nothing _) = return $ pack n
            convertSegment e@PathSegment{} = throwError "Currently, we do not support type parameters in paths.\n" <> show e

instance ConvertExpr (Block a) where
    convertExpr (Block [] _ _) = return $ LitE UnitLit
    -- TODO extend this into a higher-order function "unsafe" that we can leverage in our compiler
    --      to separate safe from unsafe parts of a program.
    convertExpr b@(Block _ Unsafe _) = throwError $ "Currently, we do not support unsafe blocks.\n" <> show b
    convertExpr (Block stmts Normal _) = 
        foldM 
            (\stmt cont -> (\e -> e cont) <$> convertStmt stmt) 
            (LitE UnitLit) 
            $ reverse stmts
    
    convertStmt :: CompM m => Stmt a -> m (FrLang.Expr -> FrLang.Expr)
    convertStmt (Local pat _ (Just e) [] _) = do
        pat' <- convertPat pat
        e' <- convertExpr e
        return $ LetE pat' e'
    convertStmt s@(Local pat _ Nothing _ _) = throwError $ "Variables bind values and as such they need to be initialized. \n" <> show s
    convertStmt s@Local{} = throwError $ "Currently, we do not support attributes on local bindings.\n" <> show s
    convertStmt s@ItemStmt = throwError $ "Currently, we do not support item statements.\n" <> show s
    convertStmt (NoSemi e _) = const <$> convertExpr e
    convertStmt (Semi e _) = StmtE <$> convertExpr e
    convertStmt s@MacStmt{} = throwError $ "Currently, we do not support macro calls.\n" <> show s

instance ConvertExpr (Stmt a) where
    -- FIXME type information is lost here
    convertExpr (Local pat _ (Just e) [] _) = do
        pat' <- convertPat pat
        e' <- convertExpr e
        return 

instance ConvertPat (Rust.Pat a) where
    convertPat (WildP _) = return $ VarP $ Binding "_"
    convertPat (IdentP (ByValue Immutable) Ident{name=n, raw=False} Nothing _) = return $ VarP $ Binding n
    convertPat p@(IdentP _ Ident{raw=True} _ _) = throwError $ "Qualified identifiers in a pattern are currently not supported. Pattern: " <> show p
    convertPat p@(IdentP _ _ (Just _) _) = throwError $ "Currently, we do not support nested patterns: " <> show p 
    convertPat p@(IdentP (ByValue Mutable) _ _ _) = throwError $ seqParProgNote <> "\n" <> show p
    convertPat p@(IdentP (ByRef _) _ _ _) = throwError $ seqParProgNote <> "\n" <> show p
    convertPat p@StructP{} = throwError $ "Currently, we do not support struct patterns: " <> show p <> ". Please use a function."
    convertPat p@TupleStructP{} = throwError $ "Currently, we do not support tuple struct patterns: " <> show p <> ". Please use a function."
    convertPat p@PathP{} = throwError $ "Currently, we do not support path patterns: " <> show p <> ". Please use a function."
    convertPat (TupleP patterns Nothing _) = return $ TupP <$> mapM converPat patterns
    convertPat p@TupleP{} = throwError $ "Currently, we do not support .. patterns: " <> show p <> "."
    convertPat p@BoxP{} = throwError $ "Currently, we do not support box patterns: " <> show p <> ". Please use a function."
    convertPat p@RefP{} = throwError $ seqParProgNote <> "\n" <> show p
    convertPat p@LitP{} = throwError $ "Currently, we do not support literal patterns: " <> show p <> ". Please use a function."
    convertPat p@RangeP{} = throwError $  "Currently, we do not support range patterns: " <> show p <> ". Please use a function."
    convertPat p@SliceP{} = throwError $  "Currently, we do not support slice patterns: " <> show p <> ". Please use a function."
    convertPat p@MacP{} = throwError $  "Currently, we do not support patterns resulting from macro expansion: " <> show p <> ". Please use a function."

instance ConvertPat (Arg a) where
    -- FIXME We certainly should have a way to attach (type) information to our expressions/patterns
    convertPat (Arg (Just p) _ _) = convertPat p
    convertPat a@(Arg Nothing _ _) = throwError $ "Currently, we require a name for each argument, not only its type. If this is a type definition in your code, then please file a bug.\n" <> show a
    convertPat a = throwError $ "Currently, we do not support self arguments. \n" <> show a

-- instance ConvertExpr Ident where
--     convertExpr Ident{name=n, raw=False} = return $ Lit $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) $ pack n) Nothing
--     -- TODO it seems we need a context here to understand the QualifiedPath.
--     --      the question is whether we really need this???
--     convertExpr Ident{name=n, raw=True} = undefined 

instance ConvertExpr BinOp where
    convertExpr AddOp = toExpr "+"
    convertExpr SubOp = toExpr "-"
    convertExpr MulOp = toExpr "*"
    convertExpr DivOp = toExpr "/"
    convertExpr RemOp = toExpr "%"
    convertExpr AndOp = toExpr "&&"
    convertExpr OrOp  = toExpr "||"
    convertExpr BitXorOp  = toExpr "^"
    convertExpr BitAndOp  = toExpr "&"
    convertExpr BitOrOp  = toExpr "|"
    convertExpr ShlOp  = toExpr "<<"
    convertExpr ShrOp  = toExpr ">>"
    convertExpr EqOp  = toExpr "=="
    convertExpr LtOp  = toExpr "<"
    convertExpr LeOp  = toExpr "<="
    convertExpr NeOp  = toExpr "!="
    convertExpr GeOp  = toExpr ">="
    convertExpr GtOp  = toExpr ">"

instance ConvertExpr UnOp where
    convertExpr Deref = toExpr "*"
    convertExpr Not   = toExpr "!"
    convertExpr Neg   = toExpr "-"

toExpr op = return $ Lit $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) op) Nothing

instance ConvertExpr (Rust.Lit a) where
    convertExpr (Int Dec i _ _) = return $ FrLang.Lit $ NumericLit i
    convertExpr _ = throwError "Currently, we miss proper support for literals. This is a TODO. Please file a bug."

seqParProgNote = "In a sequential program, memory management can be performed at compile-time via the borrowing concept. For a parallel program, this is not easily possible anymore. You will have to move your memory management from compile-time to runtime, i.e., from references to std::sync::Arc. Currently, we do not perform this conversion."