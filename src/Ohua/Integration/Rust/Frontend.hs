{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module Ohua.Integration.Rust.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Types
import Ohua.Frontend.Convert

import Ohua.Integration.Lang
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.TypeExtraction

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS


instance Integration (Language 'Rust) where
    type NS (Language 'Rust) = Module
    type Type (Language 'Rust) = (RustArgType Span)

    loadNs :: CompM m => Language 'Rust -> FilePath -> m (Module, Namespace (FrLang.Expr (RustArgType Span)))
    loadNs _ srcFile = do
        mod <- liftIO $ load srcFile
        ns <- extractNs mod
        return (Module srcFile mod, ns)
        where
            extractNs :: CompM m => SourceFile Span -> m (Namespace (FrLang.Expr (RustArgType Span)))
            -- TODO we might need to retrieve this from the file path.
            -- extractNs (SourceFile Nothing a b) = extractNs $ SourceFile (Just $ takeFileName srcFile) a b
            extractNs (SourceFile _ _ items) = do
                imports <- concat . catMaybes <$>
                        mapM 
                            (\case 
                                (Use _ _ t _) -> Just . toList <$> extractImports [] t
                                _ -> return Nothing)
                            items
                algos <- catMaybes <$>
                        mapM 
                            (\case
                                (Fn _ _ ident decl _ _ _ _ block _) -> Just <$> extractAlgo ident decl block
                                _ -> return Nothing)
                            items
                return $ Namespace (filePathToNsRef srcFile) imports algos

            extractImports :: CompM m => [Binding] -> UseTree Span -> m (NonEmpty Import)
            extractImports prefix (UseTreeSimple path (Just alias) _) = 
                (:|[]) . flip Alias (toBinding alias) . makeThrow . (prefix <>) <$> toBindings path
            extractImports _prefix u@(UseTreeSimple path Nothing _) = do
                bnds <- reverse <$> toBindings path
                case bnds of
                    [] -> throwError $ "Empty 'use' path detected. Impossible: This program certainly does not pass 'rustc'." <> show u
                    (x:xs) ->  return (Full (makeThrow $ reverse xs) x :| [])
            extractImports prefix (UseTreeGlob path _) = 
                (:|[]) . Glob . makeThrow . (prefix <>)<$> toBindings path
            extractImports prefix u@(UseTreeNested path nesteds _) = do
                path' <- (prefix <>) <$> toBindings path
                nesteds' <- case nonEmpty nesteds of
                                Just n -> return n
                                Nothing -> throwError $ "Empty nested 'use' detected. Impossible: This program certainly does not pass 'rustc'." <> show u
                join <$> mapM (extractImports path') nesteds'

            extractAlgo :: CompM m => Ident -> FnDecl Span -> Block Span -> m (Algo (FrLang.Expr (RustArgType Span)))
            extractAlgo ident (FnDecl args _ _ _) block = do
                args' <- mapM convertPat args
                block' <- convertExpr block
                return $ Algo
                            (toBinding ident) 
                            $ LamE args' block'
            
            toBindings p@(Path _ segments _) =
                forM segments $ \case
                    (PathSegment ident Nothing _) -> return $ toBinding ident
                    (PathSegment _ (Just _) _) -> throwError $ "We currently do not support import paths with path parameters.\n" <> show p
                    
    loadTypes :: CompM m => Language 'Rust -> Module -> Namespace (FrLang.Expr (RustArgType Span)) -> m (Namespace (FrLang.Expr (RustArgType Span)))
    loadTypes _ _ ohuaNs = do
        filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNs^.algos)
        types <- load $ HS.toList $ HS.fromList $ concatMap fst filesAndPaths
        types' <- HM.fromList <$> mapM (verifyAndRegister types) filesAndPaths
        updateExprs ohuaNs (transformM (assignTypes types'))
        where
            assignTypes :: CompM m => FunTypes -> FrLang.Expr (RustArgType Span) -> m (FrLang.Expr (RustArgType Span))
            assignTypes types = \case 
                (LitE (FunRefLit (FunRef qb i _))) ->
                    case HM.lookup qb types of
                        Just typ -> return $ LitE $ FunRefLit $ FunRef qb i typ
                        Nothing -> throwError "Compiler invariant broken." -- Liquid Haskell?!
                e ->  return e

            resolvedImports :: HM.HashMap Binding NSRef
            resolvedImports = HM.fromList $ mapMaybe resolveImport (ohuaNs^.imports)

            resolveImport :: Import -> Maybe (Binding, NSRef)
            resolveImport (Full ns binding) = Just (binding, ns)
            resolveImport (Alias ns alias) = Just (alias, ns)
            resolveImport (Glob _) = Nothing

            globs :: [NSRef]
            globs = mapMaybe (\case (Glob ns) -> Just ns; _ -> Nothing) (ohuaNs^.imports)
                
            funsForAlgo :: CompM m => Algo (FrLang.Expr (RustArgType Span)) -> m [([NSRef], QualifiedBinding)]
            funsForAlgo (Algo _name code) = 
                mapM lookupFunTypes [f | LitE (FunRefLit (FunRef f _ _)) <- universe code]

            lookupFunTypes :: CompM m => QualifiedBinding -> m ([NSRef], QualifiedBinding)
            lookupFunTypes q@(QualifiedBinding [] name) =
                return $ (,q) $ maybe globs (:[]) $ HM.lookup name resolvedImports
            lookupFunTypes q@(QualifiedBinding nsRef _name) = 
                let (alias:rest) = unwrap nsRef
                in case HM.lookup alias resolvedImports of
                    Just a -> return ([NSRef $ unwrap a ++ rest], q)
                    Nothing -> throwError $ "Invariant broken: I found a module alias reference that is not listed in the imports: " <> show q <> "\n Please move it into this module if you happen to import this via a mod.rs file."

            load :: CompM m => [NSRef] -> m FunTypes
            load nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".rs")) nsRefs

            verifyAndRegister :: CompM m => FunTypes -> ([NSRef], QualifiedBinding) -> m (QualifiedBinding, FunType (RustArgType Span))
            verifyAndRegister types ([candidate], qp@(QualifiedBinding _ name)) =
                case HM.lookup (QualifiedBinding candidate name) types of
                    Just t -> return (qp, t)
                    Nothing -> throwError $ "Function `" <> show name <> "` not found in module `" <> show candidate <> "`. I need these types to generate the code. Please check that the function actually exists by compiling again with `rustc`."
            verifyAndRegister types (globs, qp@(QualifiedBinding _ name)) =
                case mapMaybe ((`HM.lookup` types) . (`QualifiedBinding` name)) globs of
                    [] -> throwError $ "Function `" <> show name <> "` not found in modules `" <> show globs <> "`. I need these types to generate the code. Please check that the function actually exists by compiling again with `rustc`."
                    [t] -> return (qp, t)
                    _ -> throwError $ "Multiple definitions of function `" <> show name <> "` in modules " <> show globs <> " detected!\nPlease verify again that your code compiles properly by running `rustc`. If the problem persists then please file a bug. (See issue sertel/ohua-frontend#1)"

instance (Show a) => ConvertExpr (Rust.Expr a) where 
    convertExpr e@Box{} = throwError $ "Currently, we do not support the construction of boxed values. Please do so in a function." <> show e
    convertExpr e@InPlace{} = throwError $ "Currently, we do not support in-place expressions.\n" <> show e
    convertExpr e@Vec{} = throwError $ "Currently, we do not support array expressions. Please do so in a function.\n" <> show e
    convertExpr (Call [] fun args _) = do
        fun' <- convertExpr fun
        args' <- mapM convertExpr args
        return $ fun' `AppE` args'
    convertExpr e@Call{} = throwError $ "Currently, we do not support attributes on function calls.\n" <> show e
    convertExpr (MethodCall [] receiver Ident{name=method} Nothing args _) = do
        receiver' <- convertExpr receiver
        let method' = LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) $ fromString method) Nothing Untyped
        args' <- mapM convertExpr args
        return $ BindE receiver' method' `AppE` args'
    convertExpr e@(MethodCall [] _ _ (Just _) _ _) = throwError $ "Currently, we do not support type parameters for function calls. Your best shot: wrap the call into a function.\n" <> show e
    convertExpr e@MethodCall{} = throwError $ "Currently, we do not support attributes on method calls.\n" <> show e
    convertExpr (TupExpr [] vars _) = do
        vars' <- mapM convertExpr vars
        return $ TupE vars'
    convertExpr e@TupExpr{} = throwError $ "Currently, we do not support attributes on tuple expressions.\n" <> show e
    convertExpr (Binary [] op left right _) = do
        op' <- convertExpr op
        left' <- convertExpr left
        right' <- convertExpr right
        return $ op' `AppE` [left', right']
    convertExpr e@Binary{} = throwError $ "Currently, we do not support attributes on binary operations.\n" <> show e
    convertExpr (Unary [] op arg _) = do
        op' <- convertExpr op
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
        falseBlock' <- maybe (return $ FrLang.LitE UnitLit) convertExpr falseBlock
        return $ IfE expr' trueBlock' falseBlock'
    convertExpr e@If{} = throwError $ "Currently, we do not support attributes on conditional expressions.\n" <> show e
    convertExpr e@IfLet{} = throwError $ "Currently, we do not support if-let expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr (While [] cond block Nothing _) = do
        cond' <- convertExpr cond
        block' <- convertExpr block
        let loopLambdaRef = "while_loop_body"
        let recur = IfE 
                        cond' 
                        (VarE loopLambdaRef `AppE` [])
                        $ LitE UnitLit
        return $ 
            -- FIXME proper name generation needed here!
            LetE 
                (VarP loopLambdaRef)
                (LamE [] $ StmtE block' recur)
                recur
    convertExpr e@(While [] _ _ (Just _) _) = throwError $ "Currently, we do not support loop labels.\n" <> show e
    convertExpr e@While{} = throwError $ "Currently, we do not support attributes on while loops.\n" <> show e
    convertExpr e@WhileLet{} = throwError $ "Currently, we do not support if-let expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr (ForLoop [] pat dataExpr body Nothing _) = do
        pat' <- convertPat pat
        dataExpr' <- convertExpr dataExpr
        body' <- convertExpr body
        return $
            MapE
                (LamE [pat'] body')
                dataExpr'
    convertExpr e@(ForLoop [] _ _ _ (Just _) _) = throwError $ "Currently, we do not support loop labels.\n" <> show e
    convertExpr e@ForLoop{} = throwError $ "Currently, we do not support attributes on for loops.\n" <> show e
    convertExpr e@Loop{} = throwError $ "Currently, we do not support conditionless loops. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr e@Match{} = throwError $ "Currently, we do not support match expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
    convertExpr (Closure [] Movable Value (FnDecl args _ False _) body _) = do
        -- FIXME We are again dropping the type info here which may later on be needed in the code gen.
        args' <- mapM convertPat args
        body' <- convertExpr body
        return $ LamE args' body'
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
    convertExpr (PathExpr [] Nothing path _) = convertExpr path
    convertExpr e@(PathExpr [] (Just _) _ _) = throwError $ "Currently, we do not support paths to 'self', i.e., compilation of 'impl' functions. \n" <> show e
    convertExpr e@PathExpr{} = throwError $ "Currently, we do not support attributes on path expressions.\n" <> show e
    convertExpr e@AddrOf{} = throwError $ seqParProgNote <> "\n" <> show e
    convertExpr e@Break{} = throwError $ "Currently, we do not support 'break' expressions. Please reformulate the loop. \n" <> show e
    convertExpr e@Continue{} = throwError $ "Currently, we do not support 'continue' expressions. Please reformulate the loop. \n" <> show e
    convertExpr e@Ret{} = throwError $ "Currently, we do not support 'return' expressions. Please reformulate into an expression without a semicolon. \n" <> show e
    convertExpr e@MacExpr{} = throwError $ "Currently, we do not support macro invocations. \n" <> show e
    convertExpr e@Struct{} = throwError $ "Currently, we do not support struct literal expressions. Please use a function.\n" <> show e
    convertExpr e@Repeat{} = throwError $ "Currently, we do not support array construction expressions. Please use a function.\n" <> show e
    convertExpr e@ParenExpr{} = throwError $ "Impossible per documentation of language-rust.\n" <> show e
    convertExpr e@Try{} = throwError $ "Currently, we do not support error handling expressions. Please use a function.\n" <> show e
    convertExpr e@Yield{} = throwError $ "Currently, we do not support generator/yield expressions. Please use a function.\n" <> show e

instance (Show a) => ConvertExpr (Rust.Path a) where
    -- This needs context information to distinguish a path from a variable.
    -- A transformation is performing this disambiguation later on.
    convertExpr (Path _ segments _) = 
        case segments of 
            [segment] -> VarE . fromString <$> convertSegment segment
            segs -> do 
                segments' <- mapM convertSegment segments
                let (x:revPath) = reverse segments'
                return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow $ map fromString $ reverse revPath) $ fromString x) Nothing Untyped
        where 
            convertSegment (PathSegment Ident{name=n} Nothing _) = return n
            convertSegment e@PathSegment{} = throwError $ "Currently, we do not support type parameters in paths.\n" <> show e

instance (Show a) => ConvertExpr (Rust.Block a) where
    convertExpr (Block [] _ _) = return $ LitE UnitLit
    -- TODO extend this into a higher-order function "unsafe" that we can leverage in our compiler
    --      to separate safe from unsafe parts of a program.
    convertExpr b@(Block _ Unsafe _) = throwError $ "Currently, we do not support unsafe blocks.\n" <> show b
    convertExpr (Block stmts Rust.Normal _) = 
        foldM 
            (\cont stmt -> (\e -> e cont) <$> convertStmt stmt) 
            (LitE UnitLit) 
            $ reverse stmts
        where
            convertStmt :: CompM m => Stmt a -> m (FrLang.Expr ty -> FrLang.Expr ty)
            convertStmt (Local pat _ (Just e) [] _) = do
                pat' <- convertPat pat
                e' <- convertExpr e
                return $ LetE pat' e'
            convertStmt s@(Local pat _ Nothing _ _) = throwError $ "Variables bind values and as such they need to be initialized. \n" <> show s
            convertStmt s@Local{} = throwError $ "Currently, we do not support attributes on local bindings.\n" <> show s
            convertStmt s@ItemStmt{} = throwError $ "Currently, we do not support item statements.\n" <> show s
            convertStmt (NoSemi e _) = const <$> convertExpr e
            convertStmt (Semi e _) = StmtE <$> convertExpr e
            convertStmt s@MacStmt{} = throwError $ "Currently, we do not support macro calls.\n" <> show s

instance (Show a) => ConvertPat (Rust.Pat a) where
    convertPat (WildP _) = return $ VarP $ fromString "_"
    convertPat (IdentP (ByValue Immutable) Ident{name=n, raw=False} Nothing _) = return $ VarP $ fromString n
    convertPat p@(IdentP _ Ident{raw=True} _ _) = throwError $ "Qualified identifiers in a pattern are currently not supported. Pattern: " <> show p
    convertPat p@(IdentP _ _ (Just _) _) = throwError $ "Currently, we do not support nested patterns: " <> show p 
    convertPat p@(IdentP (ByValue Mutable) _ _ _) = throwError $ seqParProgNote <> "\n" <> show p
    convertPat p@(IdentP (ByRef _) _ _ _) = throwError $ seqParProgNote <> "\n" <> show p
    convertPat p@StructP{} = throwError $ "Currently, we do not support struct patterns: " <> show p <> ". Please use a function."
    convertPat p@TupleStructP{} = throwError $ "Currently, we do not support tuple struct patterns: " <> show p <> ". Please use a function."
    convertPat p@PathP{} = throwError $ "Currently, we do not support path patterns: " <> show p <> ". Please use a function."
    convertPat (TupleP patterns Nothing _) = TupP <$> mapM convertPat patterns
    convertPat p@TupleP{} = throwError $ "Currently, we do not support .. patterns: " <> show p <> "."
    convertPat p@BoxP{} = throwError $ "Currently, we do not support box patterns: " <> show p <> ". Please use a function."
    convertPat p@RefP{} = throwError $ seqParProgNote <> "\n" <> show p
    convertPat p@LitP{} = throwError $ "Currently, we do not support literal patterns: " <> show p <> ". Please use a function."
    convertPat p@RangeP{} = throwError $  "Currently, we do not support range patterns: " <> show p <> ". Please use a function."
    convertPat p@SliceP{} = throwError $  "Currently, we do not support slice patterns: " <> show p <> ". Please use a function."
    convertPat p@MacP{} = throwError $  "Currently, we do not support patterns resulting from macro expansion: " <> show p <> ". Please use a function."

instance (Show a) => ConvertPat (Arg a) where
    -- FIXME We certainly should have a way to attach (type) information to our expressions/patterns
    convertPat (Arg (Just p) _ _) = convertPat p
    convertPat a@(Arg Nothing _ _) = throwError $ "Currently, we require a name for each argument, not only its type. If this is a type definition in your code, then please file a bug.\n" <> show a
    convertPat a = throwError $ "Currently, we do not support self arguments. \n" <> show a

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

toExpr op = return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) op) Nothing Untyped

instance (Show a) => ConvertExpr (Rust.Lit a) where
    convertExpr (Int Dec i _ _) = return $ LitE $ NumericLit i
    convertExpr _ = throwError "Currently, we miss proper support for literals. This is a TODO. Please file a bug."

seqParProgNote = "In a sequential program, memory management can be performed at compile-time via the borrowing concept. For a parallel program, this is not easily possible anymore. You will have to move your memory management from compile-time to runtime, i.e., from references to std::sync::Arc. Currently, we do not perform this conversion."