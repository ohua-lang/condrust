{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Ohua.Integration.Rust.Frontend where

import qualified Data.HashMap.Lazy as HM
-- import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
-- import Ohua.Frontend.Convert

import Data.Text.Lazy.IO as T
import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Frontend.Lang as FrLang
    ( Expr(..),
      Pat(TupP, VarP) )
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Types
import Ohua.Integration.Lang
import qualified Ohua.Integration.Rust.Frontend.Convert as SubC
import qualified Ohua.Integration.Rust.Frontend.Subset as Sub
import Ohua.Integration.Rust.TypeExtraction as TE
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Prelude hiding (getArgType)
import qualified Control.Applicative as Hm


class ConvertExpr a where
  convertExpr :: SubC.ConvertM m => a -> m (FrLang.Expr RustArgType)

class ConvertPat a where
  convertPat :: SubC.ConvertM m => a -> m (FrLang.Pat RustArgType)

instance Integration (Language 'Rust) where
  type HostModule (Language 'Rust) = Module
  type Type (Language 'Rust) = RustArgType
  type AlgoSrc (Language 'Rust) = Item Span

  loadNs ::
    CompM m =>
    Language 'Rust ->
    FilePath ->
    m (Module, Namespace (FrLang.Expr RustArgType) (Item Span) RustArgType, Module)
  -- ToDo: If we want to include global constants in the 'type inference' for functions
  --       we need an extra pass here to collect them to the context before parsing the algos
  loadNs _ srcFile = do
    mod <- liftIO $ load srcFile
    ns <- extractNs mod
    -- REMINDER Replace empty placeholder by extracted module
    return (Module srcFile mod, ns, Module "placeholderlib.rs" placeholderModule)
    where
      extractNs ::
        CompM m =>
        SourceFile Span ->
        m (Namespace (FrLang.Expr RustArgType) (Item Span) RustArgType)

      extractNs input = do
        globalDefs <- collectGlobals input
        extractWithGlobals globalDefs input
        where
          collectGlobals input@(SourceFile _ _ items) = do
            return $ foldl (\hm (k, v) -> HM.insert k v hm) HM.empty
              (mapMaybe 
              (\case
                        (ConstItem _attr _pub ident ty expr _ )-> Just (toBinding ident, Sub.RustType (deSpan ty))
                        (Static _ _ ident ty Immutable expr span) -> Just (toBinding ident, Sub.RustType (deSpan ty))
                        (Static _ _ ident ty Mutable expr span) ->
                            error $ "You are trying to use a global mutable object "<> show ident <>" in a concurrent program ... DON'T"
                        _ -> Nothing
              )
                items)

          extractWithGlobals globs (SourceFile _ _ items) = do
            imports <-
              concat . catMaybes
                <$> mapM
                  ( \case
                      (Use _ _ t _) -> Just . toList <$> extractImports [] t
                      _ -> return Nothing
                  )
                  items
            algos <-
              catMaybes
                <$> mapM
                  ( \case
                      f@(Fn _ _ ident decl _ _ block _) ->
                        Just . (\e -> Algo (toBinding ident) e f (getReturn decl)) <$> evalStateT (extractAlgo decl block) globs
                      _ -> return Nothing
                  )
                  items
            return $ Namespace (filePathToNsRef srcFile) imports algos

      extractImports :: CompM m => [Binding] -> UseTree Span -> m (NonEmpty Import)
      -- UseTreeSimple means: 'use something;' 
      extractImports prefix (UseTreeSimple path (Just alias) _) =
        (:| []) . flip Alias (toBinding alias) . makeThrow . (prefix <>) <$> toBindings path
      extractImports prefix u@(UseTreeSimple path Nothing _) = do
        bnds <- reverse <$> toBindings path
        case bnds of
          [] -> throwError $ "Empty 'use' path detected. Impossible: This program certainly does not pass 'rustc'." <> show u
          (x : xs) -> return (Full (makeThrow $ prefix <> reverse xs) x :| [])
      -- UseTreeGlob means: 'use something::*;'
      extractImports prefix (UseTreeGlob path _) =
        (:| []) . Glob . makeThrow . (prefix <>) <$> toBindings path
      -- UseTreeNested means:'use some::thing::...' or 'use some::thing::{Items}'
      extractImports prefix u@(UseTreeNested path nesteds _) = do
        path' <- (prefix <>) <$> toBindings path
        nesteds' <- case nonEmpty nesteds of
          Just n -> return n
          Nothing -> throwError $ "Empty nested 'use' detected. Impossible: This program certainly does not pass 'rustc'." <> show u
        join <$> mapM (extractImports path') nesteds'

      extractAlgo ::
        SubC.ConvertM m =>
        FnDecl Span ->
        Block Span ->
        m (FrLang.Expr RustArgType)
      extractAlgo (FnDecl args _ _ _) block = do
        -- Add the parameters and their types to the context
        args' <- mapM (convertPat <=< SubC.convertArg) args
        -- Convert the function block
        block' <- convertIntoFrExpr block
        return $ LamE args' block'

      convertIntoFrExpr :: SubC.ConvertM m => Block Span -> m (FrLang.Expr RustArgType)
      convertIntoFrExpr rustBlock = do
        subsetExpr <- SubC.convertBlock rustBlock
        convertExpr subsetExpr

      toBindings p@(Path _ segments _) =
        forM segments $ \case
          (PathSegment ident Nothing _) -> return $ toBinding ident
          (PathSegment _ (Just _) _) -> error $ "We currently do not support import paths with path parameters.\n" <> show p
      
      getReturn :: FnDecl Span -> RustArgType
      -- FIXME: We may need more elaborate patterns for the type here
      -- Currently we only care for pure functions as algos and do nto support tuple
      -- returns (or any sort of elaborate types in the composition). If this changes, we need to 
      -- make the return type more specific
      getReturn (FnDecl _ (Just ty) _ _ ) = TE.Normal $ deSpan ty
      getReturn (FnDecl _ Nothing _ _ ) = TE.Normal TE.rustUnitReturn

  loadTypes ::
    CompM m =>
    Language 'Rust ->
    Module ->
    Namespace (FrLang.Expr RustArgType) (Item Span) RustArgType ->
    m (Namespace (FrLang.Expr RustArgType) (Item Span) RustArgType)
  loadTypes  _ (Module ownFile _) ohuaNs = return ohuaNs
   {-
  loadTypes _ (Module ownFile _) ohuaNs = do
    -- Extract namespace reference (filepath) and Qualified name for all functions called in each algo
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNs ^. algos)
    traceShowM $ "files and paths: " <> show filesAndPaths
    -- For functions from the compiled module (not imported), set the file path to the current file
    let filesAndPaths' = map (first convertOwn) filesAndPaths
    -- Go through all namespace references, parse the files and return all function types defined in there
    typez <- load $ concatMap fst filesAndPaths'
    -- traceShowM $ "loaded types: " <> show types
    -- For each function reference check the extracted function type, if function is not found or defined multiple
    -- times this will fail
    types' <- HM.fromList . catMaybes <$> mapM (verifyAndRegister typez) filesAndPaths'
    -- traceShowM $ "extracted types: " <> show types'
    -- Replace type annotations of function literals with extracted types
    updateExprs ohuaNs (transformM (assignTypes types'))

    where
      funsForAlgo :: CompM m => Algo (FrLang.Expr RustArgType) (Item Span) -> m [([NSRef], QualifiedBinding)]
      funsForAlgo (Algo _name code _) = do
        traceShowM $ "algo: " <> show _name <> "\n code: \n" <> quickRender code
        -- ToDo: Show types of functions here. They should allready be correct after transformmation to IR
        mapM lookupFunTypes [f | LitE (FunRefLit (FunRef f _ _)) <- universe code]

      lookupFunTypes :: CompM m => QualifiedBinding -> m ([NSRef], QualifiedBinding)
      lookupFunTypes q@(QualifiedBinding (NSRef []) nam) =
        return $ (,q) $ maybe globs (: []) $ HM.lookup nam fullyResolvedImports
      lookupFunTypes q@(QualifiedBinding nsRef _name) =
        let (aliaz : rest) = unwrap nsRef
         in case HM.lookup aliaz aliasImports of
              Just a -> return ([NSRef $ unwrap a ++ rest], q)
              Nothing -> case globs of
                [] ->
                  throwError $
                    "Invariant broken: I found the module alias reference '"
                      <> show q
                      <> "' that is not listed in the imports and no glob imports (::*) were defined: '"
                      <> show globs
                      <> "'\n Please move it into this module if you happen to import this via a mod.rs file."
                xs -> return (xs, q)

      convertOwn :: [NSRef] -> [NSRef]
      convertOwn [] = [filePathToNsRef ownFile]
      convertOwn n = n

      -- | Extract a HashMap of {name: function type} from the given file reference
      load :: CompM m => [NSRef] -> m FunTypes
      load nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".rs")) nsRefs

      verifyAndRegister :: CompM m => FunTypes -> ([NSRef], QualifiedBinding) -> m (Maybe (QualifiedBinding, FunType RustArgType))
      verifyAndRegister typez ([candidate], qp@(QualifiedBinding _ nam)) =
        case HM.lookup (QualifiedBinding candidate nam) typez of
          Just t -> return $ Just (qp, t)
          Nothing -> do
            $(logWarn) $ "Function `" <> show (unwrap nam) <> "` not found in module `" <> show candidate <> "`."
            return Nothing
      verifyAndRegister typez (globs', qp@(QualifiedBinding _ nam)) =
        case mapMaybe ((`HM.lookup` typez) . (`QualifiedBinding` nam)) globs' of
          [] -> do
            $(logWarn) $ "Function `" <> show (unwrap nam) <> "` not found in modules `" <> show globs' <> "`. "
            return Nothing
          [t] -> return $ Just (qp, t)
          _ -> throwError $ "Multiple definitions of function `" <> show (unwrap nam) <> "` in modules " <> show globs' <> " detected!\nPlease verify again that your code compiles properly by running `rustc`. If the problem persists then please file a bug. (See issue sertel/ohua-frontend#1)"

      assignTypes :: CompM m => FunTypes -> FrLang.Expr RustArgType -> m (FrLang.Expr RustArgType)
      -- FIXME It is nice to write it like this, really, but this has to check whether the function used in the code has (at the very least)
      --       the same number of arguments as it is applied to. If this does not match then clearly we need to error here!
      -- NO. We don't. Writing correct Rust code is the developers responsibility. We only need to extract types to
      -- implement correct transformations
      assignTypes typez = \case
        f@(LitE (FunRefLit (FunRef qb i ft))) | allArgsTyped ft -> return f
        (LitE (FunRefLit (FunRef qb i _))) ->
          case HM.lookup qb typez of
            Just typ ->
              -- do
              --     traceShowM $ "Fun: " <> show qb <> " Type: " <> show typ
              return $ LitE $ FunRefLit $ FunRef qb i typ
            Nothing -> throwError $ "I was unable to determine the types for the call to '" <> show qb <> "'. Please provide type annotations."
        e -> return e

      allArgsTyped (FunType Left {}) = True
      allArgsTyped (FunType (Right args)) = all (\case TypeVar -> False; _ -> True) args
      allArgsTyped (STFunType TypeVar _) = False
      allArgsTyped (STFunType _ Left {}) = True
      allArgsTyped (STFunType _ (Right args)) = all (\case TypeVar -> False; _ -> True) args
      allArgsTyped _ = False

      fullyResolvedImports = resolvedImports fullyResolved
      aliasImports = resolvedImports aliases

      resolvedImports :: (Import -> Maybe (Binding, NSRef)) -> HM.HashMap Binding NSRef
      resolvedImports f = HM.fromList $ mapMaybe f (ohuaNs ^. imports)

      fullyResolved :: Import -> Maybe (Binding, NSRef)
      fullyResolved (Full n bnd) = Just (bnd, n)
      fullyResolved _ = Nothing

      aliases :: Import -> Maybe (Binding, NSRef)
      aliases (Full (NSRef n) bnd) = Just (bnd, NSRef $ reverse $ bnd : reverse n)
      aliases (Alias n aliaz) = Just (aliaz, n)
      aliases _ = Nothing

      globs :: [NSRef]
      globs = mapMaybe (\case (Glob n) -> Just n; _ -> Nothing) (ohuaNs ^. imports)

    -}

argTypesFromContext :: SubC.ConvertM m => [Sub.Expr] -> m (Either Unit (NonEmpty (ArgType RustArgType)))
argTypesFromContext args =
    case args of
    [] -> return $ Left Unit
    (x : xs) -> Right <$> mapM getArgType (x :| xs)

-- If we don't want additional typing via extraction from scope we need this to succeed 
-- i.e. no 'TypeVar' results here
getArgType :: SubC.ConvertM m => Sub.Expr -> m (ArgType RustArgType)
getArgType (Sub.Var bnd) = do
  ctxt <- get
  let argtype = HM.lookup bnd ctxt
  case argtype of
    Just (Sub.RustType rustType) -> return $ Type $ TE.Normal rustType
    Nothing -> error $ "No type info found for" <> show bnd
-- FIXME: Actually literal should just carry through the value and the type of the literal
getArgType (Sub.Lit lit) = case lit of
  Sub.Bool b -> return $ Type . TE.Normal $ PathTy Nothing (Path False [PathSegment "bool" Nothing ()] ()) ()
  Sub.Int i ->  return $ Type . TE.Normal $ PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()
  -- Sub.String s ->  return $ Type . TE.Normal $ PathTy Nothing (Path False [PathSegment "String" Nothing ()] ()) ()
  -- ToDo: Add other literals
getArgType e = error $ "No type info found for" <> show e

-- ISSUE: I thought we could support Assignments as we actually just need to replcae them with ssa let bindings.
--        However the compiler does constant propagation so a `mut i:i32 = 1` will be replaced by `1` 
--        before we reach the point were it is reassigned -> I need to check if/how this can work out

instance ConvertExpr Sub.Expr where
  -- ToDo: Type function literal here
  convertExpr (Sub.Call fun args) = do
    ctxt <- get
    fun' <- convertExpr fun
    -- Function references may be parsed as simple variables as in x = f()
    -- or as path expressions as in x = std::f()
    fun'' <- case fun' of
      LitE (FunRefLit (FunRef ref _ _)) -> do
        -- traceM $ "Context at parsing function " <> show ref <> ": \n" <> show ctxt <> "\n"
        ty <- argTypesFromContext args
        return $ LitE (FunRefLit (FunRef ref Nothing $ FunType ty))
      VarE bnd ty -> do
        let qBnd = toQualBinding bnd
        -- traceM $ "Context at parsing function " <> show bnd <> ": \n" <> show ctxt <> "\n"
        -- ty <- argTypesFromContext args
        return $ LitE (FunRefLit (FunRef qBnd Nothing $ FunType ty))
      _ -> return fun'
    args' <- mapM convertExpr args
    return $ fun'' `AppE` args'
  -- ToDo: Type function literal here 
  convertExpr (Sub.MethodCall receiver (Sub.CallRef method _) args) = do
    ctxt <- get
    receiver' <- convertExpr receiver
    argTys <- argTypesFromContext args
    receiverTy <- getArgType receiver
    -- traceM $ "Context at parsing function " <> show method <> ": \n" <> show ctxt <> "\n"
    let method' = LitE (FunRefLit (FunRef method Nothing $ STFunType receiverTy argTys))
    args' <- mapM convertExpr args
    return $ BindE receiver' method' `AppE` args'
  convertExpr (Sub.Tuple vars) = do
    vars' <- mapM convertExpr vars
    argTys <- argTypesFromContext vars
    let funTy = FunType argTys
    return $ TupE funTy vars'
  convertExpr (Sub.Binary op left right) = do
    left' <- convertExpr left
    right' <- convertExpr right
    types <- argTypesFromContext [left, right]
    funref <- asFunRef (asBinSymbol op) types
    return $  AppE funref [left', right']
  convertExpr (Sub.Unary op arg) = do
    arg' <- convertExpr arg
    argTy <- argTypesFromContext [arg]
    funref <- asFunRef (asUnSymbol op) argTy
    return $ AppE funref [arg']
  convertExpr (Sub.Lit l) = convertExpr l
  convertExpr (Sub.If expr trueBlock falseBlock) = do
    expr' <- convertExpr expr
    trueBlock' <- convertExpr trueBlock
    falseBlock' <- maybe (return $ FrLang.LitE UnitLit) convertExpr falseBlock
    return $ IfE expr' trueBlock' falseBlock'
  convertExpr (Sub.While cond block) = do
    cond' <- convertExpr cond
    block' <- convertExpr block
    return $ WhileE cond' block'
{-
    let loopLambdaRef = "while_loop_body"
    let recur =
          IfE
            cond'
            (AppE (VarE loopLambdaRef)  [])
            $ LitE UnitLit
    return $
      -- FIXME proper name generation needed here!
      LetE
        (VarP loopLambdaRef)
        (LamE [] $ StmtE block' recur)
        recur
-}
  convertExpr (Sub.ForLoop pat dataExpr body) = do
    pat' <- convertPat pat
    dataExpr' <- convertExpr dataExpr
    body' <- convertExpr body
    return $
      MapE
        (LamE [pat'] body')
        dataExpr'
  convertExpr (Sub.EndlessLoop body) = do
    body' <- convertExpr body
    -- FIXME proper name generation needed here!
    --       (although there can be only one endless loop in the whole term)
    -- Basically a loop becomes :
    {-
    let endless_loop:() = (\cond -> let result = *loopbody* in if cond ) 
        in  endless_loop true
    
    -}
    let loopVar=  VarP "endless_loop" (Type rustUnitReturn) 
    let condRef = "cond"
    let resultRef = "result"
    return $
      LetE
        loopVar
        (LamE [VarP condRef] $
          LetE (VarP resultRef) body'
            (IfE
              (VarE condRef)
              (AppE (VarE loopLambdaRef)  [VarE condRef])
              $ VarE resultRef
            ))
        (AppE (VarE loopLambdaRef) [LitE $ BoolLit True])

  convertExpr (Sub.Closure _ _ _ args _retTy body) = do
    -- currently, we do not have support to pass the return type of a closure around
    ctxt <- get
    args' <- mapM convertPat args
    body' <- convertExpr body
    put ctxt -- make sure that the variables are removed again
    return $ LamE args' body'
  convertExpr (Sub.BlockExpr block) = convertExpr block
  convertExpr (Sub.PathExpr (Sub.CallRef ref tyInfo)) =
    -- TODO handle type info (these are bound type parameters)
    -- REMINDER: This is (one) origin of Function References. Check what kind of type annotations may already be 
    --           present here and incoorporate them.
    return $ LitE $ FunRefLit $ FunRef ref Nothing Untyped
  convertExpr (Sub.Var bnd ty) = return $ VarE bnd (Type ty)

instance ConvertExpr Sub.Block where
  convertExpr (Sub.RustBlock Sub.Normal stmts) =
    evalStateT (convertStmts stmts) =<< get
    where
      convertStmts [] = return $ LitE UnitLit
      convertStmts (x : xs) =
        let last = NE.last (x :| xs)
            heads = NE.init (x :| xs)
         in do
              convertedHeads <- mapM convertStmt heads
              convertedLast <- convertLastStmt last
              return $
                foldr
                  (\stmt cont -> stmt cont)
                  convertedLast
                  convertedHeads
      convertStmt :: SubC.ConvertM m => Sub.Stmt -> m (FrLang.Expr RustArgType -> FrLang.Expr RustArgType)
      convertStmt s@(Sub.Local pat ty e) = do
        case (pat, ty) of
          -- ToDo: We should move this to Rust -> Sub Conversion and set it automatically if the RHS is a literal
          (Sub.IdentP (Sub.IdentPat _mode bnd _mpat), Just ty') -> modify (HM.insert bnd ty')
          (Sub.TupP idents, Just (Sub.RustType (TupTy tys _))) -> do
            -- QUESTION: Non-matching lengths of both tuples i.e. variables and types are Rust syntay errors and
            --           as such actually not our business. Should we check anyway?
            let asSubRustType = map Sub.RustType tys
                asBindings = map fromIdent idents
                keysAndValues = zip asBindings asSubRustType
            modify (\context-> foldl (\c (a,b) ->  HM.insert a b c) context keysAndValues)
          _ -> error $ "Please type annotate targets of the assignment: " <> show s <> "\n"
        pat' <- convertPat pat
        e' <- convertExpr e
        return $ LetE pat' e'
      convertStmt (Sub.NoSemi e) = StmtE <$> convertExpr e
      convertStmt (Sub.Semi e) = StmtE <$> convertExpr e
      convertStmt Sub.StandaloneSemi = return id

      convertLastStmt e@(Sub.NoSemi expr) =
        case expr of
          Sub.ForLoop {} -> (\e -> e $ LitE UnitLit) <$> convertStmt e
          _ -> convertExpr expr
      convertLastStmt e = (\e -> e $ LitE UnitLit) <$> convertStmt e

fromIdent :: Sub.IdentPat -> Binding
fromIdent (Sub.IdentPat _mode bnd ) =  bnd

instance ConvertPat Sub.Pat where
  convertPat Sub.WildP = return $ VarP (fromString "_") TypeVar
  convertPat (Sub.IdentP ip) = convertPat ip
  convertPat (Sub.TupP patterns) = TupP <$> mapM convertPat patterns

instance ConvertPat Sub.IdentPat where
  convertPat (Sub.IdentPat mutability bnd ty) = return $ VarP bnd ty

instance ConvertPat Sub.Arg where
  convertPat (Sub.Arg pat ty) = do
    case pat of
      -- Reminder: Actually we should either remove one of the type sources
      --           or at least ensure they are equal
      Sub.IdentP (Sub.IdentPat _ bnd _pty) -> modify (HM.insert bnd ty)
      _ -> return ()
    convertPat pat

asBinSymbol:: Sub.BinOp -> Binding
asBinSymbol Sub.Add =  "+"
asBinSymbol Sub.Sub =  "-"
asBinSymbol Sub.Mul =  "*"
asBinSymbol Sub.Div =  "/"
asBinSymbol Sub.Lt =  "<"
asBinSymbol Sub.Lte =  "<="
asBinSymbol Sub.Gte =  ">="
asBinSymbol Sub.Gt =  ">"
asBinSymbol Sub.EqOp =  "=="
asBinSymbol Sub.OrOp =  "||"

asUnSymbol::Sub.UnOp -> Binding
asUnSymbol Sub.Deref =  "*"
asUnSymbol Sub.Not =  "!"
asUnSymbol Sub.Neg =  "-"

asFunRef :: Monad m => Binding -> Either Unit (NonEmpty (ArgType RustArgType)) -> m (FrLang.Expr RustArgType)
asFunRef op tys = return $
      LitE $ FunRefLit $
        FunRef (QualifiedBinding (makeThrow []) op) Nothing (FunType tys)


toQualBinding :: Binding -> QualifiedBinding
toQualBinding = QualifiedBinding (makeThrow [])

instance ConvertExpr Sub.Lit where
  convertExpr (Sub.Int i) = return $ LitE $ NumericLit i
  convertExpr (Sub.Bool b) = return $ LitE $ BoolLit b
