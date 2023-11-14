{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

module Ohua.Integration.Python.Frontend where

import Ohua.Prelude
-- import GHC.Exts
import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.TypeSystem (Delta)

import Ohua.Integration.Lang

import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeHandling 
import Ohua.Integration.Python.Frontend.Convert (suiteToSub, paramToSub)
import qualified Ohua.Integration.Python.Frontend.Subset as Sub
import qualified Ohua.Integration.Python.SpecialFunctions as SF

import Language.Python.Common (SrcSpan)
import qualified Language.Python.Common.AST as Py

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

-- | Contexts keeps track of names and types 
type Context = HM.HashMap Binding Sub.PythonType
type ConvertM m = (Monad m, MonadState Context m)


type PythonNamespace = Namespace (FrLang.UnresolvedExpr PythonVarType) (Py.Statement SrcSpan)

defaultType:: OhuaType PythonVarType Unresolved
defaultType = HType (HostType PythonObject) Nothing

instance Integration (Language 'Python) where
    type HostModule (Language 'Python) = Module
    type Type (Language 'Python) =  PythonVarType
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    {- | Loading a namespace means extracting
            a) function defintions to be complied and
            b) imported references
         from a given source file. Any other top-level statements will be
         ignored for now.
    -}
    -- REMINDER: Type of placeholder needs to be adapted here
    loadNs :: ErrAndLogM m => Language 'Python -> FilePath -> m (Module, PythonNamespace, Module)
    loadNs _ srcFile = do
            pyMod <- liftIO $ load srcFile
            modNS <- extractNs pyMod
            -- REMINDER: Next Steps replace True by
            --  a) an empty Python module and 
            --  b) the python module consisting of the extracted functions
            return (Module srcFile pyMod, modNS, Module "placeholderlib.py" placeholderModule)
            where
                extractNs :: ErrAndLogM m => Py.Module SrcSpan -> m PythonNamespace
                extractNs (Py.Module statements) = do
                    pyImports <- concat . catMaybes <$>
                            mapM
                                (\case
                                    Py.Import{import_items= impts} -> Just <$> extractImports impts
                                    Py.FromImport{from_module = modName,
                                                        from_items= items} -> Just <$> extractRelativeImports modName items
                                    _ -> return Nothing)
                                statements
                    -- ToDo: add proper extraction of gloabl assignments
                    modGlobals <- return []
                    -- ISSUE: Algo extraction needs a State Monad
                    -- During extraction we want to encapsulate non-compilable code into functions, move those to a library
                    -- and replace the code by a call to that library function. So the State of the monad needs to be of
                    -- type HostModule lang
                    pyAlgos <- catMaybes <$>
                            mapM
                                (\case
                                    fun@Py.Fun{} ->
                                        Just . (\e ->
                                            Algo
                                                (toBinding$ Py.fun_name fun)
                                                e
                                                fun) <$> extractAlgo fun modGlobals
                                    _ -> return Nothing)
                                statements
                    return $ Namespace (filePathToNsRef srcFile) pyImports modGlobals pyAlgos

                extractAlgo :: ErrAndLogM m => Py.Statement SrcSpan -> [Global] -> m (FrLang.UnresolvedExpr PythonVarType )
                extractAlgo function globs = do
                    let _globArgs = map (\(Global bnd) -> VarP bnd defaultType) globs
                    -- ToDo: replace empty context with globals filled context if needed
                    args' <- mapM ((`evalStateT` HM.empty) . subParamToIR <=< paramToSub) (Py.fun_args function)
                    block' <- ((`evalStateT` HM.empty) . subSuiteToIR <=< suiteToSub) (Py.fun_body function)
                    -- We can't add the Unit argument here before type checking because IType doesn't exist in Unresolved Expressions
                    {-let args'' = case globArgs ++ args' of
                                   [] -> VarP "_" TypeUnit :| []
                                   (x:xs) -> x :| xs-}
                    return $ LamE args' block'

                extractImports::ErrAndLogM m => [Py.ImportItem SrcSpan] -> m [Import]
                extractImports [] = throwError "Invalid: Empty import should not have passed the python parser"
                extractImports pyImports  = return $ map globOrAlias pyImports

                extractRelativeImports::ErrAndLogM m => Py.ImportRelative SrcSpan -> Py.FromItems SrcSpan -> m [Import]
                extractRelativeImports (Py.ImportRelative _numDots mDottedName _annot) fromItems =
                    case mDottedName of
                        Just names -> case fromItems of
                            Py.ImportEverything _annot -> return [Glob . makeThrow $ toBindings names]
                            -- Todo: Objects can also be imported by their 'real binding' or by alias
                            -- Which one should be the 'binding' in the Full Import?
                            -- Or can we introduce an alias also for Full Imports?
                            Py.FromItems items _annot -> return $ map (fullByNameOrAlias names) items
                        -- Question: Can we solve this by resolving the path or will this inevitably cause problems in distrb. scenario?
                        -- TODO: I realy think we need this as I've literally seen absolut import failing in 'the cloud' cause of
                            -- incompatible python paths (or dark magic :-/)
                        Nothing -> throwError  "Currently we do not support relative import paths"

    -- | This function assigns types to the functions called inside an algorithm. In Rust, it
    --   would do so, by checking the given namespace for definitions of those functions.
    --   As we currently do not 'type' any of the arguments in Python and the number of
    --   (explicit) arguments may vary for each function due to default values, we just assign
    --   a type of [PyObject] to every function call site.
    loadTypes :: ErrAndLogM m => Language 'Python ->
                    Module ->
                    PythonNamespace ->
                    m (Delta PythonVarType Resolved)
    loadTypes lang (Module _filepath _pymodule) _ohuaNS = do
        {-filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNS^.algos)
        let filesAndPaths' = map (first convertOwn) filesAndPaths
        fun_types <- typesFromNS $ concatMap fst filesAndPaths'
        types' <- HM.fromList <$> mapM (verifyAndRegister fun_types) filesAndPaths'-}
        -- ns <- updateExprs ohuaNS (transformM (assignTypes HM.empty))
        return HM.empty
        {-
        where
            funsForAlgo :: ErrAndLogM m => Algo (FrLang.UnresolvedExpr PythonVarType) (Py.Statement SrcSpan)
                    -> m [([NSRef], QualifiedBinding)]
            funsForAlgo (Algo _name code _annotation) = do
                return []


            convertOwn :: [NSRef] -> [NSRef]
            convertOwn [] = [filePathToNsRef filepath]
            convertOwn n = n

            typesFromNS :: ErrAndLogM m => [NSRef] -> m FunTypesMap
            typesFromNS nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".py") ) nsRefs

            verifyAndRegister :: ErrAndLogM m => FunTypesMap -> ([NSRef], QualifiedBinding)
                        -> m (QualifiedBinding, FunType PythonVarType)
            verifyAndRegister fun_types ([candidate], qB@(QualifiedBinding _ qBName)) = undefined
            verifyAndRegister fun_types ( _ , qB@(QualifiedBinding _ qBName)) = undefine

            assignTypes :: ErrAndLogM m => FunTypesMap -> FrLang.UnresolvedExpr PythonVarType -> m (FrLang.UnresolvedExpr PythonVarType)
            assignTypes funTypes function = case function of
                (AppE (LitE (FunRefLit (FunRef qBinding funID _))) args) ->
                    return $
                    -- Question: (To me) -> can we do better with the return type? i.e. it might be a tuple and we can know that
                         AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType (neOfPyType args) defaultType) args
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
-}

fullByNameOrAlias :: Py.DottedName SrcSpan -> Py.FromItem SrcSpan -> Import
fullByNameOrAlias dotted (Py.FromItem  ident Nothing _annot) = flip Full (toBinding ident) . makeThrow $ toBindings dotted
fullByNameOrAlias dotted (Py.FromItem ident (Just pyAlias) _annot) = flip Alias (toBinding pyAlias) . makeThrow $ toBindings dotted ++ [toBinding ident]

globOrAlias :: Py.ImportItem SrcSpan -> Import
globOrAlias  (Py.ImportItem dotted Nothing  _annot) = Glob . makeThrow $ toBindings dotted
globOrAlias  (Py.ImportItem dotted (Just pyAlias) _annot) = flip Alias (toBinding  pyAlias) . makeThrow $ toBindings dotted

subSuiteToIR :: ConvertM m => Sub.Suite -> m (FrLang.UnresolvedExpr PythonVarType)
subSuiteToIR (Sub.PySuite stmts) =
    evalStateT (convertStmts stmts) =<< get
    where
        convertStmts [] = return $ LitE UnitLit -- actually empty function blocks are not valid syntax and this should never be called
        convertStmts (sm:sms) =
            let lastS = NE.last (sm:|sms)
                heads = NE.init  (sm:|sms)
            in do
                irHeads <- mapM stmtToIR heads
                irLast <- lastStmtToIR lastS
                return $
                    foldr
                        (\stmt suite -> stmt suite) irLast irHeads
        stmtToIR:: (ConvertM m) => Sub.Stmt -> m (FrLang.UnresolvedExpr PythonVarType -> FrLang.UnresolvedExpr PythonVarType)
        stmtToIR assign@(Sub.Assign target expr) = do
            case target of
                (Sub.Single bnd) -> modify (HM.insert bnd Sub.PythonType)
                _ ->return ()
            case target of
                (Sub.Subscr _expr) -> stmtToIR (subscriptToCall assign)
                _ -> do
                        pat' <- subTargetToIR target
                        expr' <- subExprToIR expr
                        let pat'' =  case (expr', pat')  of
                            -- FIXME: We explicitely add the unit parameter to the function type in the language integration here
                            --        But unit typ args should be handled internally
                                    (LamE tars _expr, VarP bnd _ty) -> VarP bnd (FType (FunType (neOfPyType tars) defaultType))
                                    _ -> pat'
                        return $ LetE pat'' expr'

        stmtToIR stmt = StmtE <$> subStmtToIR stmt

        lastStmtToIR :: (ConvertM m) => Sub.Stmt -> m (FrLang.UnresolvedExpr PythonVarType)
        lastStmtToIR (Sub.Return maybeExpr) =
            case maybeExpr of
                    Just expr -> subExprToIR expr
                    Nothing -> return $ LitE UnitLit
        lastStmtToIR stmt = (\e -> e $ LitE UnitLit) <$> stmtToIR stmt

-- ToDo: This gives valid Python code automatically because subscripting is syntactic sugar
-- for those function. BUT: If we retranslate this in the backend, can we accidentally transform
-- more than intended
-- when we use real function names here ?
subscriptToCall :: Sub.Stmt -> Sub.Stmt
subscriptToCall (Sub.Assign (Sub.Subscr (Sub.Subscript bnd keyExpr)) expr)  =
    let funRef = Sub.Dotted bnd (toQualBinding SF.setItemFunction)
    in Sub.StmtExpr (Sub.Call funRef [Sub.Arg keyExpr, Sub.Arg expr])
subscriptToCall (Sub.StmtExpr (Sub.Subscript bnd keyExpr)) =
    let funRef = Sub.Dotted bnd (toQualBinding SF.getItemFunction)
    in Sub.StmtExpr (Sub.Call funRef [Sub.Arg keyExpr])


subStmtToIR :: ConvertM m=> Sub.Stmt -> m (FrLang.UnresolvedExpr PythonVarType)
subStmtToIR (Sub.WhileStmt _expr _suite) = error "Currently we do not support while-loops. Please use a recursion while we implement it."
    {-do
    cond <- subExprToIR expr
    suite' <- subSuiteToIR (Sub.PySuite suite)
    let loopRef = "while_loop_body"
    let recursivePart= IfE cond (AppEU (VarE loopRef defaultType) [])  (LitE UnitLit)
    return $ LetE (VarP loopRef defaultType) (LamE [] $ StmtE suite' recursivePart) recursivePart
-}

subStmtToIR (Sub.ForStmt target generator suite) = do
    target' <- subTargetToIR target
    generator' <- subExprToIR generator
    suite' <- subSuiteToIR (Sub.PySuite suite)
    return $ MapE (LamE [target'] suite') generator'

subStmtToIR (Sub.CondStmt [(cond, suite)] elseSuite) = do
    cond' <- subExprToIR cond
    suite' <- subSuiteToIR (Sub.PySuite suite)
    elseSuite' <- do
        case elseSuite of
            Nothing -> return $ LitE UnitLit
            Just block -> subSuiteToIR (Sub.PySuite block)
    return $ IfE cond' suite' elseSuite'

-- FIXME patterns are non exhaaustive here because CondStmt is not specific enough 
--       it has to be a NonEmpt of ifsAndSuits
subStmtToIR (Sub.CondStmt ifsAndSuits elseSuite) = do
    let ((ifE, suite):elifs) = ifsAndSuits
    condE <- subExprToIR ifE
    trueBranch <-  subSuiteToIR (Sub.PySuite suite)
    falseBranch <-  subStmtToIR (Sub.CondStmt elifs elseSuite)
    return $ IfE condE trueBranch falseBranch

subStmtToIR (Sub.StmtExpr expr) = subExprToIR expr
subStmtToIR Sub.Pass = return $ LitE UnitLit


subExprToIR :: ConvertM m => Sub.Expr -> m (FrLang.UnresolvedExpr PythonVarType)
subExprToIR (Sub.Var bnd) = return $ VarE bnd defaultType
subExprToIR (Sub.Int int) = return $ LitE $ NumericLit int
subExprToIR (Sub.Bool bl) = return $ LitE $ BoolLit bl
subExprToIR Sub.None = return $  LitE  UnitLit

subExprToIR (Sub.Call (Sub.Pure bnd) args) = do
    -- args'<- FrLang.unitArgs <$> mapM subArgToIR args
    args'<- mapM subArgToIR args
    -- FIXME: again we're constructing a function type with the unti argument added, this leaks internals and shouldn't happen in the integration
    let argTypes = neOfPyType args'
    funLit <- toFunRefLit bnd (argTypes, defaultType)
    return $ AppE funLit args'
subExprToIR (Sub.Call (Sub.Dotted objBnd (QualifiedBinding _nsref funBnd)) args) = do
    args' <- mapM subArgToIR args
     -- FIXME: again we're constructing a function type with the unti argument added, this leaks internals and shouldn't happen in the integration
    let argTypes = listOfPyType args
    return $ StateFunE (VarE objBnd defaultType) (MethodUnres funBnd) args'
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
    -- We cannot do this unitParams thing (i.e. adding a unit param if params are empty), because this unit params only live in Resolved
    -- params' <- FrLang.unitParams <$> mapM subParamToIR params
    params' <-  mapM subParamToIR params
    expr' <- subExprToIR expr
    put ctxt
    return $ LamE params' expr'
subExprToIR (Sub.Tuple exprs) = do
    -- We cannot do unitArgs here, reson is the same as for params 
    -- exprs' <- FrLang.unitArgs <$> mapM subExprToIR exprs
    exprs' <- mapM subExprToIR exprs
    tupleCall <- toFunRefLit SF.tupleConstructor (neOfPyType exprs', defaultType)
    return $ AppE tupleCall exprs'
subExprToIR (Sub.List exprs) = do
    -- again .. no unit args
    exprs' <- mapM subExprToIR exprs
    listCall <- toFunRefLit SF.listConstructor (neOfPyType exprs', defaultType)
    return $ AppE listCall exprs'
-- | Mapping d = {1:2, 3:4} to d = dict(((1,2), (3,4)))
subExprToIR (Sub.Dict mappings) = do
     -- again .. no unit args
    exprs' <-  mapM (\(k,v) -> subExprToIR $ Sub.Tuple [k,v]) mappings
    dictCall <- toFunRefLit SF.dictConstructor (neOfPyType exprs', defaultType)
    return $ AppE dictCall exprs'
subExprToIR (Sub.Set  exprs) = do
     -- again .. no unit args
    exprs' <-  mapM subExprToIR exprs
    setCall <- toFunRefLit SF.setConstructor (neOfPyType exprs', defaultType)
    return $ AppE setCall exprs'
subExprToIR subExpr@(Sub.Subscript _bnd _expr) = do
    -- FIXME: This is to unspecific. We can only get a StmtExpr here (because its the only Python Statement jsut wrapping and expression),
    --        yet we get a warning for incomplete patterns -> introduce a separate wrapper in the subset instead of using a StmtExpr?
    let (Sub.StmtExpr call) = subscriptToCall (Sub.StmtExpr subExpr)
    subExprToIR call

mappingToTuple ::ConvertM m => (Sub.Expr, Sub.Expr) -> m (FrLang.UnresolvedExpr PythonVarType)
mappingToTuple (key, val) = do
    key' <- subExprToIR key
    val' <- subExprToIR val
    return $ FrLang.TupE (key' :|[val'])

subArgToIR :: ConvertM m => Sub.Argument -> m ( FrLang.UnresolvedExpr PythonVarType)
subArgToIR (Sub.Arg expr) = subExprToIR expr

subParamToIR :: ConvertM m => Sub.Param -> m (FrLang.Pat PythonVarType Unresolved)
subParamToIR (Sub.Param bnd) = do
    modify (HM.insert bnd Sub.PythonType)
    -- We do not need typing in Python so everything gets the default 'python object' type
    return $ VarP bnd defaultType

subTargetToIR :: ConvertM m => Sub.Target -> m (FrLang.Pat PythonVarType Unresolved)
subTargetToIR (Sub.Single bnd) = return $ VarP bnd defaultType
subTargetToIR (Sub.Tpl (b:bnds)) =
    let v = VarP b defaultType
        vars = map (\bnd -> VarP bnd defaultType) bnds
    in return $ TupP (v:| vars)

subBinOpToIR :: ConvertM m => Sub.BinOp -> m ( FrLang.UnresolvedExpr PythonVarType)
subBinOpToIR Sub.Plus = toFunRefLit "+" simpleBinarySignature
subBinOpToIR Sub.Minus = toFunRefLit "-" simpleBinarySignature
subBinOpToIR Sub.Multiply = toFunRefLit "*" simpleBinarySignature
subBinOpToIR Sub.Divide = toFunRefLit "/" simpleBinarySignature
subBinOpToIR Sub.FloorDivide = toFunRefLit "//" simpleBinarySignature
subBinOpToIR Sub.Modulo = toFunRefLit "%" simpleBinarySignature
subBinOpToIR Sub.Exponent = toFunRefLit "**" simpleBinarySignature
subBinOpToIR Sub.MatrixMult = toFunRefLit "@" simpleBinarySignature

subBinOpToIR Sub.And = toFunRefLit "and" simpleBinarySignature
subBinOpToIR Sub.Or  = toFunRefLit "or" simpleBinarySignature
subBinOpToIR Sub.In = toFunRefLit "in" simpleBinarySignature
subBinOpToIR Sub.Is = toFunRefLit "is" simpleBinarySignature
subBinOpToIR Sub.IsNot = toFunRefLit "is not" simpleBinarySignature
subBinOpToIR Sub.NotIn = toFunRefLit "not in" simpleBinarySignature

subBinOpToIR Sub.LessThan = toFunRefLit "<"  simpleBinarySignature
subBinOpToIR Sub.GreaterThan = toFunRefLit ">" simpleBinarySignature
subBinOpToIR Sub.Equality  = toFunRefLit "==" simpleBinarySignature
subBinOpToIR Sub.GreaterThanEquals = toFunRefLit ">=" simpleBinarySignature
subBinOpToIR Sub.LessThanEquals = toFunRefLit "<=" simpleBinarySignature
subBinOpToIR Sub.NotEquals = toFunRefLit "!=" simpleBinarySignature

subBinOpToIR Sub.BinaryAnd = toFunRefLit "&" simpleBinarySignature
subBinOpToIR Sub.BinaryOr = toFunRefLit "|" simpleBinarySignature
subBinOpToIR Sub.Xor = toFunRefLit "^" simpleBinarySignature
subBinOpToIR Sub.ShiftLeft = toFunRefLit "<<" simpleBinarySignature
subBinOpToIR Sub.ShiftRight = toFunRefLit ">>" simpleBinarySignature


subUnOpToIR :: ConvertM m => Sub.UnOp -> m ( FrLang.UnresolvedExpr PythonVarType)
subUnOpToIR Sub.Not  = toFunRefLit "not" simpleUnarySignature
subUnOpToIR Sub.Invert = toFunRefLit "~" simpleUnarySignature


listOfPyType :: [a] -> [OhuaType PythonVarType Unresolved] 
listOfPyType args = map (const defaultType) args

neOfPyType ::  [a] -> NonEmpty (OhuaType PythonVarType Unresolved)
neOfPyType [] = defaultType :| []
neOfPyType (_:xs) = (defaultType :| map (const defaultType) xs)



{- | Turns given string representation into literal expression representig an untyped,
     'unscoped' (the empty list in as Binding argument) function reference
-}
toFunRefLit :: Monad m => Binding -> (NonEmpty (OhuaType PythonVarType Unresolved), OhuaType PythonVarType Unresolved) -> m (FrLang.UnresolvedExpr PythonVarType)
toFunRefLit funBind (argTys, retTy) =
  return $
  LitE $ FunRefLit $
  FunRef (QualifiedBinding (makeThrow []) funBind) Nothing $ FunType argTys retTy

simpleBinarySignature :: (NonEmpty (OhuaType PythonVarType Unresolved), OhuaType PythonVarType Unresolved)
simpleBinarySignature = (defaultType :| [defaultType], defaultType)

simpleUnarySignature :: (NonEmpty (OhuaType PythonVarType Unresolved), OhuaType PythonVarType Unresolved)
simpleUnarySignature = (defaultType :| [], defaultType)

toBindings:: [Py.Ident a] -> [Binding]
toBindings = map toBinding
