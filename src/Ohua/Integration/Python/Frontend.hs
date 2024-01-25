{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

module Ohua.Integration.Python.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.TypeSystem (Delta)

import Ohua.Integration.Lang

import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeHandling as TH
import Ohua.Integration.Python.Frontend.Convert (suiteToSub, paramToSub)
import qualified Ohua.Integration.Python.Frontend.Subset as Sub


import Language.Python.Common (SrcSpan)
import qualified Language.Python.Common.AST as Py

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

-- | Contexts keeps track of names and types 
type Context = HM.HashMap Binding Sub.PythonType
type ConvertM m = (Monad m, MonadState Context m)


type PythonNamespace = Namespace (FrLang.UnresolvedExpr PythonVarType) (Py.Statement SrcSpan) (OhuaType PythonVarType 'Resolved)


instance Integration (Language 'Python) where
    type HostModule (Language 'Python) = Module
    type Type (Language 'Python) =  PythonVarType
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

    {- | Loading a namespace means extracting
            a) function defintions to be compiled and
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
                                        Just . (\(expr, ty) ->
                                            Algo
                                                (toBinding$ Py.fun_name fun)
                                                ty
                                                expr
                                                fun) <$> extractAlgo fun modGlobals
                                    _ -> return Nothing)
                                statements
                    return $ Namespace (filePathToNsRef srcFile) pyImports modGlobals pyAlgos

                extractAlgo :: ErrAndLogM m => Py.Statement SrcSpan -> [Global] -> m (FrLang.UnresolvedExpr PythonVarType, OhuaType PythonVarType 'Resolved)
                extractAlgo function globs = do
                    let _globArgs = map (\(Global bnd) -> VarP bnd defaultType) globs
                    -- ToDo: replace empty context with globals filled context if needed
                    args' <- mapM ((`evalStateT` HM.empty) . subParamToIR <=< paramToSub) (Py.fun_args function)
                    block' <- ((`evalStateT` HM.empty) . subSuiteToIR <=< suiteToSub) (Py.fun_body function)
                    -- ToDo: make globals explicit arguments
                    {-let args'' = case globArgs ++ args' of
                                   [] -> VarP "_" TypeUnit :| []
                                   (x:xs) -> x :| xs-}
                    return (LamE args' block', FType (FunType (asInputTypes args') defaultType))

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

    -- | In Python we do not actually type things. But we need to provide function types for the resolution step.
    --   For pure function calls this work by annotating the functions with resolved types upon lowering. For methods this
    --   doesn't work, because methods are jsut Bindings and carry no type information. Hence we need to
    --   transfer type infromation for methods via the Delta context (This step would be done in Rust as well. But the type information there
    --   is exptracted from the compilation scope, while we skip the scope extraction in Python)
    loadTypes :: ErrAndLogM m => Language 'Python ->
                    Module ->
                    PythonNamespace ->
                    m (Delta PythonVarType Resolved)
    loadTypes lang (Module _filepath _pymodule) ohuaNS = do
        methodCalls <- concat <$> mapM methods (ohuaNS^.algos)
        let defaults_and_used = foldr (\(qb, ty) hm -> HM.insert qb ty hm) TH.defaultMethods methodCalls
        return defaults_and_used
        where
            methods::ErrAndLogM m => Algo (FrLang.UnresolvedExpr PythonVarType) (Py.Statement SrcSpan) (OhuaType PythonVarType 'Resolved) -> m [(QualifiedBinding, FunType PythonVarType Resolved)]
            methods (Algo _name _ty frlangCode _inputCode ) = do 
                mapM asMethodType $ [ (m, args) | StateFunE _obj m args <- flattenU frlangCode]
            
            asMethodType::ErrAndLogM m =>  (MethodRepr PythonVarType 'Unresolved, [FrLang.UnresolvedExpr PythonVarType]) -> m (QualifiedBinding, FunType PythonVarType Resolved)
            asMethodType ((MethodUnres bnd), args) = return ((asQualified bnd), (STFunType defaultType (asInputTypes args) defaultType))

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
                _ -> return ()
            case target of
                (Sub.Subscr _expr) -> stmtToIR (subscriptToCall assign)
                _ -> do
                        pat' <- subTargetToIR target
                        let retTy = case pat' of 
                                VarP _ _ -> defaultType
                                TupP (p:|ps) -> pythonTupleType (p:ps) 
                        expr' <- subExprToIR retTy expr
                        let pat'' =  case (expr', pat')  of
                                    (LamE tars _expr, VarP bnd _ty) -> VarP bnd (FType (FunType (asInputTypes tars) defaultType))
                                    _ -> pat'
                        return $ LetE pat'' expr'

        stmtToIR stmt = StmtE <$> subStmtToIR defaultType stmt

        lastStmtToIR :: (ConvertM m) => Sub.Stmt -> m (FrLang.UnresolvedExpr PythonVarType)
        lastStmtToIR (Sub.Return maybeExpr) =
            case maybeExpr of
                -- FIXME: We need the algo return type here
                    Just expr -> subExprToIR defaultType expr
                    Nothing -> return $ LitE UnitLit
        lastStmtToIR stmt = (\e -> e $ LitE UnitLit) <$> stmtToIR stmt

-- ToDo: This gives valid Python code automatically because subscripting is syntactic sugar
-- for those function. BUT: If we retranslate this in the backend, can we accidentally transform
-- more than intended
-- when we use real function names here ?
subscriptToCall :: Sub.Stmt -> Sub.Stmt
subscriptToCall (Sub.Assign (Sub.Subscr (Sub.Subscript bnd keyExpr)) expr)  =
    let funRef = Sub.Dotted bnd (toQualBinding TH.setItemFunction)
    in Sub.StmtExpr (Sub.Call funRef [Sub.Arg keyExpr, Sub.Arg expr])
subscriptToCall (Sub.StmtExpr (Sub.Subscript bnd keyExpr)) =
    let funRef = Sub.Dotted bnd (toQualBinding TH.getItemFunction)
    in Sub.StmtExpr (Sub.Call funRef [Sub.Arg keyExpr])


subStmtToIR :: ConvertM m => OhuaType PythonVarType 'Unresolved ->  Sub.Stmt -> m (FrLang.UnresolvedExpr PythonVarType)
subStmtToIR _t (Sub.WhileStmt _expr _suite) = error "Currently we do not support while-loops. Please use a recursion while we implement it."
    {-do
    cond <- subExprToIR expr
    suite' <- subSuiteToIR (Sub.PySuite suite)
    let loopRef = "while_loop_body"
    let recursivePart= IfE cond (AppEU (VarE loopRef defaultType) [])  (LitE UnitLit)
    return $ LetE (VarP loopRef defaultType) (LamE [] $ StmtE suite' recursivePart) recursivePart
-}

subStmtToIR t (Sub.ForStmt target generator suite) = do
    target' <- subTargetToIR target
    generator' <- subExprToIR t generator
    suite' <- subSuiteToIR (Sub.PySuite suite)
    return $ MapE (LamE [target'] suite') generator'

subStmtToIR _t (Sub.CondStmt [(cond, suite)] elseSuite) = do
    cond' <- subExprToIR defaultType cond
    suite' <- subSuiteToIR (Sub.PySuite suite)
    elseSuite' <- do
        case elseSuite of
            Nothing -> return $ LitE UnitLit
            Just block -> subSuiteToIR (Sub.PySuite block)
    return $ IfE cond' suite' elseSuite'

-- FIXME patterns are non exhaaustive here because CondStmt is not specific enough 
--       it has to be a NonEmpt of ifsAndSuits
subStmtToIR t (Sub.CondStmt ifsAndSuits elseSuite) = do
    let ((ifE, suite):elifs) = ifsAndSuits
    condE <- subExprToIR t ifE
    trueBranch <-  subSuiteToIR (Sub.PySuite suite)
    falseBranch <-  subStmtToIR t (Sub.CondStmt elifs elseSuite)
    return $ IfE condE trueBranch falseBranch

subStmtToIR _t (Sub.StmtExpr expr) = subExprToIR defaultType expr
subStmtToIR _t Sub.Pass = return $ LitE UnitLit


subExprToIR :: ConvertM m => OhuaType PythonVarType 'Unresolved -> Sub.Expr -> m (FrLang.UnresolvedExpr PythonVarType)
subExprToIR _t  (Sub.Var bnd) = return $ VarE bnd defaultType
subExprToIR _t  (Sub.Int int) = return $ LitE $ NumericLit int
subExprToIR _t  (Sub.Bool bl) = return $ LitE $ BoolLit bl
subExprToIR _t  Sub.None = return $  LitE  UnitLit

subExprToIR t (Sub.Call (Sub.Pure bnd) args) = do
    args'<- mapM (subArgToIR t) args
    let argTypes = asInputTypes args'
    funLit <- toFunRefLit bnd (argTypes, t)
    return $ AppE funLit args'
subExprToIR t (Sub.Call (Sub.Dotted objBnd (QualifiedBinding _nsref funBnd)) args) = do
    args' <- mapM (subArgToIR t) args
    return $ StateFunE (VarE objBnd defaultType) (MethodUnres funBnd) args'
subExprToIR t (Sub.Call (Sub.Direct lambdaExpr) args) = do
    args' <- mapM (subArgToIR t) args
    fun <- (subExprToIR t) lambdaExpr
    return $ AppE fun args'
subExprToIR t  (Sub.CondExpr condE trueExpr falseExpr) = do
    cond <- subExprToIR defaultType condE
    true <- subExprToIR t trueExpr
    false <- subExprToIR t falseExpr
    return $ IfE cond true false
subExprToIR _t (Sub.BinaryOp binOp expr1 expr2) = do
    op' <- subBinOpToIR binOp
    expr1' <- subExprToIR defaultType expr1
    expr2' <- subExprToIR defaultType expr2
    return $ op' `AppE` [expr1', expr2']
subExprToIR _t (Sub.UnaryOp unOp expr1) = do
    op' <- subUnOpToIR unOp
    expr1' <- subExprToIR defaultType expr1
    return $ op' `AppE` [expr1']
subExprToIR t (Sub.Lambda params expr) = do
    ctxt <- get
    params' <-  mapM subParamToIR params
    -- FIXME: THis needs to be param -> t, not t
    expr' <- subExprToIR t expr
    put ctxt
    return $ LamE params' expr'
subExprToIR t (Sub.Tuple exprs) = do
    exprs' <- mapM (subExprToIR defaultType) exprs
    tupleCall <- toFunRefLit TH.tupleConstructor (asInputTypes exprs', pythonTupleType exprs )
    return $ AppE tupleCall exprs'
subExprToIR _t (Sub.List exprs) = do
    exprs' <- mapM (subExprToIR defaultType) exprs
    listCall <- toFunRefLit TH.listConstructor (asInputTypes exprs', defaultType)
    return $ AppE listCall exprs'
-- | Mapping d = {1:2, 3:4} to d = dict(((1,2), (3,4)))
subExprToIR t (Sub.Dict mappings) = do
    exprs' <-  mapM (\(k,v) -> subExprToIR t $ Sub.Tuple [k,v]) mappings
    dictCall <- toFunRefLit TH.dictConstructor (asInputTypes exprs', defaultType)
    return $ AppE dictCall exprs'
subExprToIR _t (Sub.Set  exprs) = do
    exprs' <-  mapM (subExprToIR defaultType) exprs
    setCall <- toFunRefLit TH.setConstructor (asInputTypes exprs', defaultType)
    return $ AppE setCall exprs'
subExprToIR t subExpr@(Sub.Subscript _bnd _expr) = do
    -- FIXME: This is to unspecific. We can only get a StmtExpr here (because its the only Python Statement jsut wrapping and expression),
    --        yet we get a warning for incomplete patterns -> introduce a separate wrapper in the subset instead of using a StmtExpr?
    let (Sub.StmtExpr call) = subscriptToCall (Sub.StmtExpr subExpr)
    subExprToIR t call

subArgToIR :: ConvertM m => OhuaType PythonVarType 'Unresolved -> Sub.Argument -> m ( FrLang.UnresolvedExpr PythonVarType)
subArgToIR t (Sub.Arg expr) = subExprToIR t expr

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

asInputTypes ::  [a] -> Either () (NonEmpty (OhuaType PythonVarType res))
asInputTypes [] = Left ()
asInputTypes (_:xs) = Right (defaultType :| map (const defaultType) xs)



{- | Turns given string representation into literal expression representig an untyped,
     'unscoped' (the empty list in as Binding argument) function reference
-}
toFunRefLit :: Monad m => Binding -> (Either () (NonEmpty (OhuaType PythonVarType Unresolved)), OhuaType PythonVarType Unresolved) -> m (FrLang.UnresolvedExpr PythonVarType)
toFunRefLit funBind (argTys, retTy) =
  return $
  LitE $ FunRefLit $
  FunRef (QualifiedBinding (makeThrow []) funBind) $ FunType argTys retTy

simpleBinarySignature :: (Either () (NonEmpty (OhuaType PythonVarType Unresolved)), OhuaType PythonVarType Unresolved)
simpleBinarySignature = (Right $ defaultType :| [defaultType], defaultType)

simpleUnarySignature :: (Either () (NonEmpty (OhuaType PythonVarType Unresolved)), OhuaType PythonVarType Unresolved)
simpleUnarySignature = (Right $ defaultType :| [], defaultType)

toBindings:: [Py.Ident a] -> [Binding]
toBindings = map toBinding
