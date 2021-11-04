{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|Slowly move frontend covnersion to this file withou messing up the 
old frontend to much
|-}
module Ohua.Integration.Python.Frontend where

import Ohua.Prelude
import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang

import Ohua.Integration.Lang

import Ohua.Integration.Python.Util
import Ohua.Integration.Python.Types
import Ohua.Integration.Python.TypeExtraction
import Ohua.Integration.Python.Frontend.Convert (suiteToSub, paramToSub)
import qualified Ohua.Integration.Python.Frontend.Subset as Sub

import Language.Python.Common (SrcSpan (SpanEmpty))
import qualified Language.Python.Common.AST as Py

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

-- Keep track of names and types 
-- > will be useful when/if annotated types are passed through
type Context = HM.HashMap Binding Sub.PythonType
type ConvertM m = (Monad m, MonadState Context m)


type PythonNamespace = Namespace (FrLang.Expr PythonArgType) (Py.Statement SrcSpan)

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) =  PythonArgType
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    loadNs :: CompM m => Language 'Python -> FilePath -> m (Module, PythonNamespace)
    loadNs _ srcFile = do
            mod <- liftIO $ load srcFile
            ns <- extractNs mod
            return (Module srcFile mod, ns)
            where
                extractNs :: CompM m => Py.Module SrcSpan -> m PythonNamespace
                extractNs (Py.Module statements) = do
                    imports <- concat . catMaybes <$>
                            mapM
                                (\case
                                    imp@Py.Import{import_items= impts} -> Just <$> extractImports impts
                                    frImp@Py.FromImport{from_module = modName,
                                                        from_items= items} -> Just <$> extractRelativeImports modName items
                                    _ -> return Nothing)
                                statements
                    algos <- catMaybes <$>
                            mapM
                                (\case
                                    fun@Py.Fun{} ->
                                        Just . (\e -> Algo (toBinding$ Py.fun_name fun) e fun) <$> extractAlgo fun
                                    _ -> return Nothing)
                                statements
                    return $ Namespace (filePathToNsRef srcFile) imports algos

                extractAlgo :: CompM m => Py.Statement SrcSpan -> m (FrLang.Expr PythonArgType )
                extractAlgo function = do
                    args' <- mapM ((`evalStateT` HM.empty) . subParamToIR <=< paramToSub) (Py.fun_args function)
                    block' <- ((`evalStateT` HM.empty) . subSuiteToIR <=< suiteToSub) (Py.fun_body function)
                    return $ LamE args' block'

                extractImports::CompM m => [Py.ImportItem SrcSpan] -> m [Import]
                -- TODO: Normal imports are Globs, imports with an 'as' are Alias
                -- > Full imports are allways Py.RelativeImport
                extractImports [] = throwError "Invalid: Empty import should not have passed the python parser"
                extractImports imports  = return $ map globOrAlias imports

                extractRelativeImports::CompM m => Py.ImportRelative SrcSpan -> Py.FromItems SrcSpan -> m [Import]
                extractRelativeImports imp@(Py.ImportRelative numDots mDottedName annot) fromItems =
                    case mDottedName of
                        Just names -> case fromItems of
                            Py.ImportEverything annot -> return [Glob . makeThrow $ toBindings names]
                            -- Question: Objects can also be imported by their 'real binding' or by alias
                            -- Which one should be the 'binding' in the Full Import?
                            -- OOr can we introduce an alias also for Full Imports?
                            Py.FromItems items annot -> return $ map (fullByNameOrAlias names) items
                        -- Question: Can we solve this by resolving the path or will this inevitably cause problems in distrb. scenario?
                        -- TODO: I realy think we need this as I've literally seen absolut import failing in 'the cloud' cause of
                            -- incompatible python paths (or dark magic :-/)
                        Nothing -> throwError  "Currently we do not support relative import paths"

    loadTypes :: CompM m => Language 'Python ->
                    Module ->
                    PythonNamespace ->
                    m PythonNamespace
    -- TODO: Can meanwhile be replaced by id function
    loadTypes lang (Module filepath pymodule) ohuaNS = do
        -- Alles was vor update epressions steht ist dafür da, functionstypen aus deklarartionen (aus versch. Dateien im comilation scope zu popeln)
        -- _> ich hole mir die typen aus den call und kann mir daher den ersten Teil erstmal sparen
        filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNS^.algos)
        let filesAndPaths' = map (first convertOwn) filesAndPaths
        fun_types <- typesFromNS $ concatMap fst filesAndPaths'
        types' <- HM.fromList <$> mapM (verifyAndRegister fun_types) filesAndPaths'
        updateExprs ohuaNS (transformM (assignTypes types'))
        where
            funsForAlgo :: CompM m => Algo (FrLang.Expr PythonArgType) (Py.Statement SrcSpan)
                    -> m [([NSRef], QualifiedBinding)]
            -- extracts function literals from code and extracts for each the function
            -- type ()
            funsForAlgo (Algo _name code annotation) = do
                return []


            convertOwn :: [NSRef] -> [NSRef]
            convertOwn [] = [filePathToNsRef filepath]
            convertOwn n = n

            typesFromNS :: CompM m => [NSRef] -> m FunTypes
            typesFromNS nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".py") ) nsRefs

            verifyAndRegister :: CompM m => FunTypes -> ([NSRef], QualifiedBinding)
                        -> m (QualifiedBinding, FunType PythonArgType)
            verifyAndRegister fun_types ([candidate], qB@(QualifiedBinding _ qBName)) = undefined
            -- TODO: Can this happen and what to do then?
            verifyAndRegister fun_types ( _ , qB@(QualifiedBinding _ qBName)) = undefined

            assignTypes :: CompM m => FunTypes -> FrLang.Expr PythonArgType -> m (FrLang.Expr PythonArgType)
            assignTypes funTypes function = case function of
                (AppE (LitE (FunRefLit (FunRef qBinding funID _))) args) ->
                    return $
                         AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ listofPyType args) args
                    {- 
                    case args of
                        -- Note: In Rust this type assignment happens based on the function definitions, while the
                        -- Python integration does this based on function calls right now.
                        -- Therefore contrary to the Rust way, args might be empty here.
                        -- TODO: When I return to type extraction from defintions, make non-empty args an invariant again
                        {- [] -> throwError "Empty call unfilled."
                        --[LitE UnitLit] -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args-}
                        (a:args') ->
                            return $
                                AppE (LitE $ FunRefLit (FunRef qBinding funID FunType $ (listofPyType args))) args
                        _ -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args
                    -}
                e ->  return e

            globs :: [NSRef]
            globs = mapMaybe (\case (Glob n) -> Just n; _ -> Nothing) (ohuaNS^.imports)

fullByNameOrAlias :: Py.DottedName SrcSpan -> Py.FromItem SrcSpan -> Import
fullByNameOrAlias dotted (Py.FromItem  ident Nothing annot) = flip Full (toBinding ident) . makeThrow $ toBindings dotted
-- TODO: What about aliasing fully qualified imports??
fullByNameOrAlias dotted (Py.FromItem  ident (Just alias) annot) = undefined

globOrAlias :: Py.ImportItem SrcSpan -> Import
globOrAlias  (Py.ImportItem dotted Nothing  annot) = Glob . makeThrow $ toBindings dotted
globOrAlias  (Py.ImportItem dotted (Just alias) annot) = flip Alias (toBinding alias) . makeThrow $ toBindings dotted

subSuiteToIR::ConvertM m => Sub.Suite -> m (FrLang.Expr PythonArgType)
subSuiteToIR (Sub.PySuite stmts) =
    evalStateT (convertStmts stmts) =<< get
    where
        convertStmts [] = return $ LitE UnitLit -- actually empty function blocks are not valid syntax and this should never be called
        convertStmts (sm:sms) =
            let last = NE.last (sm:|sms)
                heads = NE.init  (sm:|sms)
            in do
                irHeads <- mapM stmtToIR heads
                irLast <- lastStmtToIR last
                return $
                    foldr
                        (\stmt suite -> stmt suite) irLast irHeads
        stmtToIR:: (ConvertM m) => Sub.Stmt -> m (FrLang.Expr PythonArgType -> FrLang.Expr PythonArgType)
        stmtToIR assign@(Sub.Assign target expr) = do
            case target of
                (Sub.Single bnd) -> modify (HM.insert bnd Sub.PythonType)
                _ ->return ()
            pat' <- subTargetToIR target
            expr' <- subExprToIR expr
            return $ LetE pat' expr'
        stmtToIR stmt = StmtE <$> subStmtToIR stmt
        lastStmtToIR :: (ConvertM m) => Sub.Stmt -> m (FrLang.Expr PythonArgType)
        lastStmtToIR ret@(Sub.Return maybeExpr) =
            case maybeExpr of
                    Just expr -> subExprToIR expr
                    Nothing -> return $ LitE UnitLit
        lastStmtToIR stmt = (\e -> e $ LitE UnitLit) <$> stmtToIR stmt

subStmtToIR :: ConvertM m=> Sub.Stmt -> m (FrLang.Expr PythonArgType)
subStmtToIR (Sub.WhileStmt expr suite) = do
    cond <- subExprToIR expr
    suite' <- subSuiteToIR suite
    let loopRef = "while_loop_body"
    let recursivePart= IfE cond (AppE (VarE loopRef) [])  (LitE UnitLit)
    return $ LetE (VarP loopRef) (LamE [] $ StmtE suite' recursivePart) recursivePart

subStmtToIR (Sub.ForStmt target generator suite) = do
    targets' <- subTargetToIR target
    generator' <- subExprToIR generator
    suite <- subSuiteToIR suite
    return $ MapE (LamE [targets'] suite) generator'

subStmtToIR (Sub.CondStmt [(cond, suite)] elseSuite) = do
    cond' <- subExprToIR cond
    suite' <- subSuiteToIR suite
    elseSuite' <- subSuiteToIR elseSuite
    return $ IfE cond' suite' elseSuite'

subStmtToIR (Sub.CondStmt ifsAndSuits elseSuite) = do
    let ((ifE, suite):elifs) = ifsAndSuits
    condE <- subExprToIR ifE
    trueBranch <-  subSuiteToIR suite
    falseBranch <-  subStmtToIR (Sub.CondStmt elifs elseSuite)
    return $ IfE condE trueBranch falseBranch

subStmtToIR (Sub.StmtExpr expr) = subExprToIR expr
subStmtToIR Sub.Pass = return $ LitE UnitLit
-- subStmtToIR any = convError any

-- subExprToIR ::ConvertM m => Sub.Expr -> m (FrLang.Expr PythonArgType)
subExprToIR :: ConvertM m => Sub.Expr -> m (FrLang.Expr PythonArgType)
subExprToIR (Sub.Var bnd) = return $ VarE bnd
subExprToIR (Sub.Int int) = return $ LitE $ NumericLit int
subExprToIR (Sub.Bool bool) = return $ LitE $ BoolLit bool
subExprToIR Sub.None = return $  LitE  UnitLit

subExprToIR (Sub.Call (Sub.Pure bnd) args) = do
    args'<- mapM subArgToIR args
    return $ AppE (VarE bnd) args'

subExprToIR (Sub.Call (Sub.Dotted objBnd funBnd) args) = do
    args' <- mapM subArgToIR args
    let receiver = VarE objBnd
        receiverTy = Type PythonObject
        argTypes = listofPyType args
        method = LitE (FunRefLit (FunRef funBnd Nothing $ STFunType receiverTy argTypes))
    return $ BindE receiver method `AppE` args'

subExprToIR (Sub.Call (Sub.Direct lambdaExpr) args) = do
    args' <- mapM subArgToIR args
    fun <- subExprToIR lambdaExpr
    return $ AppE fun args'

subExprToIR (Sub.CondExpr condE trueExpr falseExpr) = do
    cond <- subExprToIR condE
    true <- subExprToIR trueExpr
    false <- subExprToIR falseExpr
    return $ IfE cond true false

subExprToIR (Sub.BinaryOp binOp expr1 expr2) = do
    op' <- subBinOpToIR binOp
    expr1' <- subExprToIR expr1
    expr2' <- subExprToIR expr2
    return $ op' `AppE` [expr1', expr2']

subExprToIR (Sub.UnaryOp unOp expr1) = do
    op' <- subUnOpToIR unOp
    expr1' <- subExprToIR expr1
    return $ op' `AppE` [expr1']

subExprToIR (Sub.Lambda params expr) = do
    ctxt <- get 
    params' <- mapM subParamToIR params 
    expr' <- subExprToIR expr
    put ctxt
    return $ LamE params' expr'

subExprToIR (Sub.Tuple exprs) = do
    exprs' <- mapM subExprToIR exprs
    return $ TupE exprs'

-- subExprToIR any =  convError any

subArgToIR :: ConvertM m => Sub.Argument -> m ( FrLang.Expr PythonArgType)
subArgToIR (Sub.Arg expr) = subExprToIR expr

subParamToIR::ConvertM m => Sub.Param -> m FrLang.Pat
subParamToIR (Sub.Param bnd) = do
    modify (HM.insert bnd Sub.PythonType)
    return $ VarP bnd

subTargetToIR :: ConvertM m => Sub.Target -> m FrLang.Pat
subTargetToIR (Sub.Single bnd) = return $ VarP bnd
subTargetToIR (Sub.Tpl bnds) =
    let vars = map VarP bnds
    in return $ TupP vars

subBinOpToIR:: ConvertM m => Sub.BinOp -> m ( FrLang.Expr PythonArgType)
subBinOpToIR Sub.Plus = toFunRefLit "+"
subBinOpToIR Sub.Minus = toFunRefLit "-"
subBinOpToIR Sub.Multiply = toFunRefLit "*"
subBinOpToIR Sub.Divide = toFunRefLit "/"
subBinOpToIR Sub.FloorDivide = toFunRefLit "//"
subBinOpToIR Sub.Modulo = toFunRefLit "%"
subBinOpToIR Sub.Exponent = toFunRefLit "**"
subBinOpToIR Sub.MatrixMult = toFunRefLit "@"

subBinOpToIR Sub.And = toFunRefLit "and"
subBinOpToIR Sub.Or  = toFunRefLit "or"
subBinOpToIR Sub.In = toFunRefLit "in"
subBinOpToIR Sub.Is = toFunRefLit "is"
subBinOpToIR Sub.IsNot = toFunRefLit "is not"
subBinOpToIR Sub.NotIn = toFunRefLit "not in"

subBinOpToIR Sub.LessThan = toFunRefLit "<"
subBinOpToIR Sub.GreaterThan = toFunRefLit ">"
subBinOpToIR Sub.Equality  = toFunRefLit "=="
subBinOpToIR Sub.GreaterThanEquals = toFunRefLit ">="
subBinOpToIR Sub.LessThanEquals = toFunRefLit "<="
subBinOpToIR Sub.NotEquals = toFunRefLit "!="

subBinOpToIR Sub.BinaryAnd = toFunRefLit "&"
subBinOpToIR Sub.BinaryOr = toFunRefLit "|"
subBinOpToIR Sub.Xor = toFunRefLit "^"
subBinOpToIR Sub.ShiftLeft = toFunRefLit "<<"
subBinOpToIR Sub.ShiftRight = toFunRefLit ">>"


subUnOpToIR:: ConvertM m => Sub.UnOp -> m ( FrLang.Expr PythonArgType)
subUnOpToIR Sub.Not  = toFunRefLit "not"
subUnOpToIR Sub.Invert = toFunRefLit "~"


listofPyType :: [a] -> Either Unit (NonEmpty (ArgType PythonArgType))
listofPyType [] = Left Unit
listofPyType (a:args') = Right $ map (const $ Type PythonObject) (a:|args')


toFunRefLit :: Monad m => Binding -> m (FrLang.Expr PythonArgType)
{-- Note: Turns given string representation into literal expression representig an untyped, 
'unscoped' (the empty list in as Binding argument) function reference 
-- Question: why untyped ? --}
toFunRefLit string_repr = return $
                        LitE $ FunRefLit $
                        FunRef (QualifiedBinding (makeThrow []) string_repr) Nothing Untyped

convError any = throwError $ "This shouldn't happen. Please file a bug about"
                <>" missing conversion for the subset expression: " <> show any

toBindings = map toBinding

