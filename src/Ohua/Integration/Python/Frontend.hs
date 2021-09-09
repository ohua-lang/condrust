{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}

module Ohua.Integration.Python.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Types
import Ohua.Frontend.Convert
import Ohua.Frontend.PPrint ()

import Ohua.Integration.Lang
import Ohua.Integration.Python.Types
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeExtraction

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty))

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE



type PythonNamespace = Namespace (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) =  PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

-- TODO: Important -> Reassignments (x += 1, x = x + 1)
-- Note: Produces namespace later refered to with/required as 'ohuaNS^.algos' and 'ohuaNS^.imports'
-- Todo: 1. PythonSubset als data anlegen
-- Todo : 2. Python AST auf Subset Mappen 
-- Todo : 3 Subset auf IR mappen 
-- => loadNS sollte 3°2 = 3(2()) => 3 . 2 sein
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

                extractAlgo :: CompM m => Py.Statement SrcSpan -> m (FrLang.Expr (PythonArgType SrcSpan))
                extractAlgo function = do
                    args' <- mapM convertPat (Py.fun_args function)
                    block' <- convertExpr (Py.fun_body function)
                    return $ LamE args' block'

                extractImports::CompM m => [Py.ImportItem SrcSpan] -> m [Import]
                -- TODO: Normal imports are Glos, imports with an 'as' are Alias
                -- > Full imports are allways Py.RelativeImport
                extractImports [] = throwError "Invalid: Empty import should not have passed the pytho parser"
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
            funsForAlgo :: CompM m => Algo (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)
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
                        -> m (QualifiedBinding, FunType (PythonArgType SrcSpan))
            verifyAndRegister fun_types ([candidate], qB@(QualifiedBinding _ qBName)) = undefined
            -- TODO: Can this happen and what to do then?
            verifyAndRegister fun_types ( _ , qB@(QualifiedBinding _ qBName)) = undefined

            assignTypes :: CompM m => FunTypes -> FrLang.Expr (PythonArgType SrcSpan) -> m (FrLang.Expr (PythonArgType SrcSpan))
            assignTypes funTypes function = case function of
                (AppE (LitE (FunRefLit (FunRef qBinding funID _))) args) ->
                    case args of
                        -- Note: In Rust this type assignment happens based on the function definitions, while the
                        -- Python integration does this based on function calls right now.
                        -- Therefore contrary to the Rust way, args might be empty here.
                        -- TODO: When I return to type extraction from defintions, make non-empty args an invariant again
                        {- [] -> throwError "Empty call unfilled."
                        --[LitE UnitLit] -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args-}
                        (a:args') ->
                            return $
                                AppE (LitE $ FunRefLit (FunRef qBinding funID (listofPyType args))) args
                        _ -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args
                e ->  return e

            listofPyType :: [FrLang.Expr (PythonArgType SrcSpan)] -> FunType (PythonArgType SrcSpan)
            listofPyType [] = error "Empty call unfilled."
            listofPyType (a:args') = FunType $ Right $ map (const $ Type $ PythonObject noSpan) (a:|args')
            

            globs :: [NSRef]
            globs = mapMaybe (\case (Glob n) -> Just n; _ -> Nothing) (ohuaNS^.imports)

fullByNameOrAlias :: Py.DottedName SrcSpan -> Py.FromItem SrcSpan -> Import
fullByNameOrAlias dotted (Py.FromItem  ident Nothing annot) = flip Full (toBinding ident) . makeThrow $ toBindings dotted 
-- TODO: What about aliasing fully qualified imports??
fullByNameOrAlias dotted (Py.FromItem  ident (Just alias) annot) = undefined 

globOrAlias :: Py.ImportItem SrcSpan -> Import
globOrAlias  (Py.ImportItem dotted Nothing  annot) = Glob . makeThrow $ toBindings dotted
globOrAlias  (Py.ImportItem dotted (Just alias) annot) = flip Alias (toBinding alias) . makeThrow $ toBindings dotted

toBindings = map toBinding 

    {-instance ConvertPat ...Turns out, pattern matching is under way \o/ (PEP 634)-}

instance ConvertPat (Py.Parameter SrcSpan) where
    -- Question: There's a FIXME in Rusts argument conversion to attach (type) info 
    -- > why can't we make VarP have an additional Maybe ?  
    convertPat params@Py.Param{param_name=ident} = return $ VarP $ toBinding ident
    -- Question: Variables can be anything anyway. 
    -- We'll have to tread them like objects as they'r only 'frozen at the surface'. 
    -- So just prepend '*'/'**' to their names (to transfer unpacking to backend)? 
    convertPat params@Py.VarArgsPos{param_name=ident} = return $ VarP $ fromString $ "*"++Py.ident_string ident
    convertPat params@Py.VarArgsKeyword{param_name=ident} = return $ VarP $ fromString $ "**"++Py.ident_string ident
    -- Question: I found out what EndPositional acutally is. It might be there and it needs to be mapped to nothing
    -- i.e. not a fail, but realy nothing. How to do this? 
    -- Meanwhile I will just not support it
    convertPat params@Py.EndPositional{} = unsupError "keyword-only markers as arguments" params
    convertPat tplParam@Py.UnPackTuple{} = unsupError " python 2 tuple unpacking parameters (consult PEP 3113)" tplParam

--Todo: What else can be pattern in python?
{- From the py grammar:
target          ::=  identifier
                     | "(" target_list ")"
                     | "[" target_list "]"
                     | attributeref
                     | subscription
                     | slicing
                     | "*" target
-}
instance ConvertPat (Py.Expr SrcSpan) where
    -- Question: why are wilcards not different from normal vars i.e. also a VarP ... Could I deref them anywhere ?
    convertPat Py.Var {var_ident=ident, expr_annot=_expr_annot} = return $ VarP $ toBinding ident
    convertPat lst@(Py.List [expr] annot) = convertPat expr
    convertPat lst@(Py.List exprs annot) = do
        patterns <- mapM convertPat exprs
        return $ TupP patterns
    convertPat lst@(Py.Tuple exprs annot) = do
        patterns <- mapM convertPat exprs
        return $ TupP patterns
    -- Question: Rust implementation doesn't support PathP, which I assume to be attribute
    -- assingment (i.e. ~ Py.Dot). Why?
    convertPat dot@(Py.Dot exprs termVar annot) = unsupError "attribute assignment" dot
    -- Question: I assume it's troublesome for some reason to translate this (probably because in haskell 
    -- we do not modify things but return new ones)...Why exactly?
    convertPat subScr@(Py.Subscript subscriptee indexExpr annot) = unsupError "indexed patterns" subScr
    convertPat slice@(Py.SlicedExpr subscriptee slices annot) = unsupError "slice patterns" slice
    convertPat starred@(Py.Starred expr annot) = unsupError "starred expression patterns" starred
    convertPat any = throwError $ "Encountered " <> show any <> " while trying to convert patterns. This is a bug"

instance ConvertExpr (Py.Expr SrcSpan) where
    convertExpr Py.Var{var_ident= ident} = return $ VarE $ toBinding ident
    convertExpr (Py.Int val strRepr annot) = return $ LitE $ NumericLit val
    convertExpr lL@(Py.LongInt val strRepr annot) = unsupError "LongInts" lL
    convertExpr fL@(Py.Float valDbl strRepr annot) = unsupError "Floats" fL
    convertExpr imL@(Py.Imaginary valDbl strRepr annot) = unsupError "Imaginaries" imL
    convertExpr bL@(Py.Bool bool annot) = unsupError "Boolean Literals" bL    
    convertExpr (Py.None annot) = return $ LitE UnitLit 
    convertExpr ellL@(Py.Ellipsis annot) = unsupError "Ellipsis Literals" ellL
    convertExpr bsL@(Py.ByteStrings bStrings annot) = unsupError "ByteString Literals" bsL
    convertExpr strsL@(Py.Strings strings annot) = unsupError "Strings Literals" strsL
    convertExpr ustrL@(Py.UnicodeStrings strings annot) = unsupError "UnicodeStrings Literals" ustrL
    convertExpr (Py.Call fun args annot)= do
        fun' <- convertExpr fun
        args' <- mapM convertExpr args
        return $ fun' `AppE` args'
    convertExpr subSc@(Py.Subscript subscripted subscript annot) = unsupError "Subscript Expressions" subSc
    convertExpr slice@(Py.SlicedExpr sliced slices annot) = unsupError "Slicing Expressions" slice
    convertExpr condExpr@(Py.CondExpr branch ifExpr elseBranch annot) = do
        condE <- convertExpr ifExpr
        trueBranch <- convertExpr branch 
        falseBranch <- convertExpr elseBranch
        return $ IfE condE trueBranch falseBranch
    convertExpr (Py.BinaryOp operator left right annot) = do
        op' <- convertExpr operator
        left' <- convertExpr left
        right' <- convertExpr right
        return $ op' `AppE` [left', right']
    convertExpr (Py.UnaryOp operation arg annot) = do
        op' <- convertExpr operation
        arg' <- convertExpr arg
        return $ op' `AppE` [arg']
    convertExpr dot@(Py.Dot object attribute annot) = unsupError "Dot Expressions" dot
    convertExpr lam@(Py.Lambda params expr annot) = unsupError "Lambda Expressions" lam
    
    convertExpr yield@(Py.Yield mayBeArg annot) = unsupError "Yield Expressions" yield
    convertExpr gen@(Py.Generator comprehension annot) = undefined 
    convertExpr await@(Py.Await expr annot) = undefined 
    
    -- TODO/Note: It would at first glance be possible to translate lists, sets and dictioniries to tuples
    -- In a way, this enforces 'functional' usage of them i.e. recreating insted of mutating
    --But: Problems in Frontend/for Transormations
        -- Slicing with nth would not work for dicts (I could build a workarround maybe for slice expressions in the frontend)
        -- Appending works differently for sets (no duplicates)
    -- Problems in Backend: 
        -- I can not distiguish Tuples from 'Tupled-Containers' and calls
        --  like l.pop(), l.items(), l.values(), l.intersect() will fail  
    convertExpr tpl@(Py.Tuple items annot) = do
        exprs <- mapM convertExpr items
        return $ TupE exprs

    convertExpr list@(Py.List items annot) = undefined
    convertExpr listComp@(Py.ListComp comprehension annot) = undefined 
    -- TODO: Could be converted to a list of tuples
    -- ...but how could we distiguish real lists of tuples from dicts in the backend :-(
    convertExpr dict@(Py.Dictionary keysAndValues annot) = undefined 
    convertExpr dictComp@(Py.DictComp compehension annot) = undefined 
    -- TODO: Could be a list, but we'd loose distinction in the backend as with dicts :-(
    convertExpr set@(Py.Set items annot) = undefined 
    convertExpr setComp@(Py.SetComp comprehension annot) = undefined 
    -- I think supporting this could get complicated if we want to have any controle over types
    convertExpr starred@(Py.Starred expr annot) = unsupError "Starred Expressions" starred
    -- TODO/Question: I need to know all possible occurences of parenthesized expressionss and 
    -- in how far there can be precedence or other semantic issues arrising from just using the inner expression
    convertExpr paren@(Py.Paren expr annot) = convertExpr expr
    convertExpr strConv@(Py.StringConversion expr annot) = py2Error strConv

instance  ConvertExpr (Py.Argument SrcSpan) where
    convertExpr Py.ArgExpr{arg_expr=expr} = convertExpr expr
    convertExpr a@Py.ArgVarArgsPos{arg_expr=expr} = unsupError "*args in class construction" a
    convertExpr a@Py.ArgVarArgsKeyword {arg_expr=_arg_expr} = unsupError "**kwargs in class construction" a
    convertExpr a@Py.ArgKeyword{arg_keyword= varN, arg_expr= expr} = unsupError "keyword arguments in class constructors" a

-- TODO: Actually I think, no statemtement should be toplevel and handled here ...
-- Either it's 1) an import 2) a function defnition 3) a class definition and therefore handled in 
instance ConvertExpr (Py.Statement SrcSpan) where
    convertExpr Py.Import{import_items=items} = throwError "'import' should be handles elsewhere"
    convertExpr Py.FromImport{from_module= mod, from_items=items} = throwError "'from .. import' should be handles elsewhere"
    --TODO: Fail on non-emmpty else-block 
    convertExpr whileE@(Py.While cond do_block [] annE) = do
        cond' <- convertExpr cond
        block' <- convertExpr do_block
        let loopRef = makeLoopRef "while_loop_body" annE
        let loopLambdaRef = "while_loop_body"
        let recur = IfE
                        cond'
                        (VarE loopLambdaRef `AppE` [])
                        $ LitE UnitLit
        return $
            LetE
                (VarP loopLambdaRef)
                (LamE [] $ StmtE block' recur)
                recur
    convertExpr whileE@(Py.While cond do_block elseBlock annE) = unsupError "else blocks in while expressions" whileE
    --TODO: Fail on non-emmpty else-block 
    convertExpr forE@(Py.For targets generator body [] annot) = do
        patterns <- mapM convertPat targets
        generator' <- convertExpr generator
        body' <- convertExpr body
        return $
            MapE
                (LamE patterns body')
                generator'
    convertExpr forE@(Py.For targets generator body elseBlock annot) = unsupError "else blocks in for expressions" forE
    convertExpr asyncFor@(Py.AsyncFor stmt annot) = undefined
    convertExpr funDef@(Py.Fun name params mResultAnnot body annot) = throwError $ "No function definitions expected here" <> show funDef
    convertExpr asyncFun@Py.AsyncFun{} = unsupError "async function definitions" asyncFun
    convertExpr classDef@(Py.Class cName cArgs cBody annot) = unsupError "class defintions inside functions" classDef
    -- TODO: At top level this can be if __name__ == '__main__' and needs to be translated to a
    -- function, otherwise we might not want to allow code that is executed upon importing
    {-Note: there's 2 complications with if's in python 
        1st: there's elIfs -> this probably not hard, it just means i have to nes translation
        2nd: there'r blocks inside and again, I can not exclude 'return' statements in python 
        -> I assume rust function execution continues, when an if-block return a value, this is not the case in python 
    
    -}
    convertExpr ifElifElse@(Py.Conditional [(cond, branch)] elseBranch annot) = do 
        condE <- convertExpr cond
        trueBranch <- convertExpr branch 
        falseBranch <- convertExpr elseBranch
        return $ IfE condE trueBranch falseBranch

    convertExpr ifElifElse@(Py.Conditional condsAndBodys elseBranch annot) = do
        let ((ifE, suite):elifs) = condsAndBodys
        condE <- convertExpr ifE 
        trueBranch <-  convertExpr suite 
        falseBranch <-  convertExpr (Py.Conditional elifs elseBranch annot)
        return $ IfE condE trueBranch falseBranch
        {--do 
        case condsAndBodys of 
            [(cond, branch)] 
            
        (cond, branch) <- head condsAndBodys
        condE <- convertExpr cond
        trueBranch <- convertExpr branch 
        falseBranch <- convertExpr elseBranch
        return $ IfE condE trueBranch falseBranch--}

    convertExpr assign@(Py.Assign targets exor annot) = throwError $ "assignments should be handled in blocks" <> show assign
    convertExpr augmAs@(Py.AugmentedAssign target operation expr annot) = unsupError "augmented assignments" augmAs
    convertExpr annotAs@(Py.AnnotatedAssign targetAnnot target expr stmtAnnot) = unsupError "annotated assignments" annotAs
    convertExpr dec@(Py.Decorated decorators funOrClass annot) = unsupError "decorators" dec
    convertExpr ret@(Py.Return optReturn annot) = throwError $ "Please only return at the end of functions"<> show ret
    convertExpr try@(Py.Try block excepts elseBlock finallyBlock annot)= undefined
    convertExpr raise@(Py.Raise raiseExor annot) = undefined
    convertExpr with@(Py.With contextTuples block annot) = undefined
    convertExpr asyncWith@(Py.AsyncWith stmt annot) = undefined
    -- Todo: is it valid to translate 'pass' to 'UnitLit'?
    {- 'pass' as a function body -> equiv. to 'return None' -> works
       'pass' TL -> not relevant, we only look inside algos
       'pass' in a class defintion -> we don't touch those either, so that should be ok
       'pass' in a branch -> in the backend, the only point where 'UnitLit' is translated to
         'return None' or just 'return' is the end of a function block and even there we 
         could just write 'None'
    - > Seems legit
    -}
    convertExpr (Py.Pass annot) = return $ LitE UnitLit 
    convertExpr (Py.Break annot) = undefined
    convertExpr (Py.Continue annot) = undefined
    convertExpr (Py.Delete deleteExprs annot) = undefined
    convertExpr e@(Py.StmtExpr expr annot) = convertExpr expr
    -- TODO: We will probably never support this
    convertExpr e@(Py.Global globalVars annot) = unsupError "global keyword" e
    convertExpr e@(Py.NonLocal nonlocalVars annot) = unsupError "nonlocal keyword" e
    convertExpr e@(Py.Assert assertions annot) = unsupError "assertions" e
    convertExpr e@(Py.Print hasChevron args isCOmmaTrailed annot) = py2Error e
    convertExpr e@(Py.Exec expr optionalGlobalsLocals annot) = unsupError "exec statements" e


makeLoopRef :: String -> SrcSpan -> String
-- TODO: Case we like the idea, propagate ScrSpan => a throug all fknts and
-- produce name based on coords here. 
-- Alternative make case distinction here if we may want to change the annotations
makeLoopRef loopKind loc = loopKind ++ "_"



instance  ConvertExpr (Py.Suite SrcSpan) where
    convertExpr statements =
         case statements of
            [] -> throwError "Empty function body. Actually that shouldn't have passed the parser"
            (x:xs) ->
                let last = NE.head $ NE.reverse $ x:|xs
                    heads = NE.tail $ NE.reverse $ x:|xs
                in do
                    convertedLast <- convertLastStmt last
                    foldM
                     (\cont stmt -> (\e -> e cont) <$> convertStmt stmt)
                     convertedLast
                     heads
                where
                    convertStmt :: (CompM m) => Py.Statement SrcSpan -> m (FrLang.Expr ty -> FrLang.Expr ty)
                    convertStmt assign@(Py.Assign [target] expr annot) = do
                        pat' <- convertPat target
                        expr' <- convertExpr expr
                        return $ LetE pat' expr'
                    -- TODO/Question: The Error produced by this translation of Py.Assign was
                    -- [Error] Unsupported multiple outputs: Destruct (Direct (DataBinding (Binding "x_0_0")) :| [])
                    -- The reason was that assigning to tuples is obviously a part of the language but functions are not intended to output 
                    -- tuples i.e. no assignments to TupP 
                    -- SO Question 1: Why, what's the prupose of it?
                    -- Question 2: (Rather Todo) How to handle python's unpacking here?
                    -- Note: I can try the following:
                    {-Input:  x,y,z = f()
                     -Output: Let tpl = f() in
                                let x = tpl[0] in
                                    let y = tpl[1] in 
                                        let z = tpl[2] in
                                            .. do stuff ... -}
                    {-
                    convertStmt assign@(Py.Assign targets expr annot) = do
                        pat' <- mapM convertPat targets
                        expr' <- convertExpr expr
                        return $ LetE (TupP pat') expr'
                    -}    
                    {-convertStmt augAssign@(Py.AugmentedAssign target operation expr annot) = do
                        -- TODO: I need an outer let here ..
                        -- x += 1
                        Let tmp = x 
                            in let x = tmp + 1
                               in x
                       
                        pat' <- convertPat target
                        let binOp = Py.BinaryOp (toBinOp operation) target expr noSpan
                        expr' <- convertExpr binOp
                        return $ LetE pat' expr'
                     -}
                    -- Question: I understand return statements are not supported in Rust, 
                    -- as they are optional there and might exit functions at different points, right?
                    -- So I'd assume that I should only support them as the last statement in a block right?                 
                    convertStmt stmt = StmtE <$> convertExpr stmt

                    -- Cases for last statement 
                        -- -> either it's a return statement with an expression 
                            -- this should be equivalent to NoSemi in Rust 
                            -- > return the converted expression 
                        -- or it's an empty return statement
                            -- this should be equivalent to having a Semi Statement last in Rust
                            -- except that ther's no statement to translate before -> 
                            -- return LitE UnitLit
                        -- or it's just any statement
                            -- convert the statement and append LitE UnitLit
                    
                    convertLastStmt :: (CompM m) => Py.Statement SrcSpan -> m (FrLang.Expr ty)
                    convertLastStmt ret@(Py.Return maybeExpr annot) =
                        case maybeExpr of
                             Just expr -> convertExpr expr
                             Nothing -> return $ LitE UnitLit
                    convertLastStmt stmt = (\e -> e $ LitE UnitLit) <$> convertStmt stmt

instance ConvertExpr (Py.Op SrcSpan) where
    convertExpr Py.Plus{} = toExpr "+"
    convertExpr Py.Minus{} = toExpr "-"
    convertExpr Py.Multiply{} = toExpr "*"
    convertExpr Py.Divide{} = toExpr "/"
    convertExpr Py.FloorDivide{} = toExpr "//"
    convertExpr Py.Modulo{} = toExpr "%"
    convertExpr Py.Exponent{} = toExpr "**"
    convertExpr Py.MatrixMult{} = toExpr "@"

    convertExpr Py.And{} = toExpr "and"
    convertExpr Py.Or{}  = toExpr "or"
    convertExpr Py.Not{}  = toExpr "not"
    convertExpr Py.In{} = toExpr "in"
    convertExpr Py.Is{} = toExpr "is"
    convertExpr Py.IsNot{} = toExpr "is not"
    convertExpr Py.NotIn{} = toExpr "not in"

    convertExpr Py.LessThan{} = toExpr "<"
    convertExpr Py.GreaterThan{} = toExpr ">"
    convertExpr Py.Equality{}  = toExpr "=="
    convertExpr Py.GreaterThanEquals{} = toExpr ">="
    convertExpr Py.LessThanEquals{} = toExpr "<="
    convertExpr Py.NotEquals{} = toExpr "!="

    convertExpr Py.BinaryAnd{} = toExpr "&"
    convertExpr Py.BinaryOr{} = toExpr "|"
    convertExpr Py.Xor{} = toExpr "^"
    convertExpr Py.ShiftLeft{} = toExpr "<<"
    convertExpr Py.ShiftRight{} = toExpr ">>"
    convertExpr Py.Invert{} = toExpr "~"

    convertExpr e@Py.NotEqualsV2{} = py2Error e

toBinOp :: Py.AssignOp SrcSpan -> Py.Op SrcSpan
toBinOp Py.PlusAssign{} = Py.Plus noSpan
toBinOp Py.MinusAssign{} = Py.Minus noSpan
toBinOp Py.MultAssign{} = Py.Multiply noSpan
toBinOp Py.DivAssign{} = Py.Divide noSpan
toBinOp Py.ModAssign{} = Py.Modulo noSpan
toBinOp Py.PowAssign{} = Py.Exponent noSpan
toBinOp Py.BinAndAssign{} = Py.BinaryAnd noSpan
toBinOp Py.BinOrAssign{} = Py.BinaryOr noSpan
toBinOp Py.BinXorAssign{} = Py.Xor noSpan
toBinOp Py.LeftShiftAssign{} = Py.ShiftLeft noSpan
toBinOp Py.RightShiftAssign{} = Py.ShiftRight noSpan
toBinOp Py.FloorDivAssign{} = Py.FloorDivide noSpan
toBinOp Py.MatrixMultAssign{} = Py.MatrixMult noSpan


toExpr :: Monad m => Binding -> m (FrLang.Expr ty)
{-- Note: Turns given string representation into literal expression representig an untyped, 
'unscoped' (the empty list in as Binding argument) function reference 
-- Question: why untyped ? --}
toExpr string_repr = return $
                        LitE $ FunRefLit $
                        FunRef (QualifiedBinding (makeThrow []) string_repr) Nothing Untyped


unsupError text expr = throwError $ "Currently we do not support "<> text <>" used in: "<> show expr

--TODO: can this be my responsibility in any way or redirect to bjpop@csse.unimelb.edu.au here ?
py2Error expr = throwError $ "For whatever reason you managed to get the exclusively version 2 expression "
                                <> show expr <> " through the python3 parser of language-python."

