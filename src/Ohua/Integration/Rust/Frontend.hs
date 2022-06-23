{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Integration.Rust.Frontend where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
-- import Ohua.Frontend.Convert

import Data.Text.Lazy.IO as T
import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Types
import Ohua.Integration.Lang
import qualified Ohua.Integration.Rust.Frontend.Convert as SubC
import qualified Ohua.Integration.Rust.Frontend.Subset as Sub
import Ohua.Integration.Rust.TypeExtraction as TE
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Prelude

type Context = HM.HashMap Binding Sub.RustType

type ConvertM m = (Monad m, MonadState Context m)

class ConvertExpr a where
  convertExpr :: ConvertM m => a -> m (FrLang.Expr RustArgType)

class ConvertPat a where
  convertPat :: ConvertM m => a -> m FrLang.Pat

instance Integration (Language 'Rust) where
  type NS (Language 'Rust) = Module
  type Type (Language 'Rust) = RustArgType
  type AlgoSrc (Language 'Rust) = Item Span

  loadNs ::
    CompM m =>
    Language 'Rust ->
    FilePath ->
    m (Module, Namespace (FrLang.Expr RustArgType) (Item Span), Bool)
  loadNs _ srcFile = do
    mod <- liftIO $ load srcFile
    ns <- extractNs mod
    -- REMINDER Replace True by extracted module
    return (Module srcFile mod, ns, True)
    where
      extractNs ::
        CompM m =>
        SourceFile Span ->
        m (Namespace (FrLang.Expr RustArgType) (Item Span))
      -- TODO we might need to retrieve this from the file path.
      -- extractNs (SourceFile Nothing a b) = extractNs $ SourceFile (Just $ takeFileName srcFile) a b
      extractNs (SourceFile _ _ items) = do
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
                    Just . (\e -> Algo (toBinding ident) e f) <$> extractAlgo decl block
                  _ -> return Nothing
              )
              items
        return $ Namespace (filePathToNsRef srcFile) imports algos

      extractImports :: CompM m => [Binding] -> UseTree Span -> m (NonEmpty Import)
      extractImports prefix (UseTreeSimple path (Just alias) _) =
        (:| []) . flip Alias (toBinding alias) . makeThrow . (prefix <>) <$> toBindings path
      extractImports _prefix u@(UseTreeSimple path Nothing _) = do
        bnds <- reverse <$> toBindings path
        case bnds of
          [] -> throwError $ "Empty 'use' path detected. Impossible: This program certainly does not pass 'rustc'." <> show u
          (x : xs) -> return (Full (makeThrow $ reverse xs) x :| [])
      extractImports prefix (UseTreeGlob path _) =
        (:| []) . Glob . makeThrow . (prefix <>) <$> toBindings path
      extractImports prefix u@(UseTreeNested path nesteds _) = do
        path' <- (prefix <>) <$> toBindings path
        nesteds' <- case nonEmpty nesteds of
          Just n -> return n
          Nothing -> throwError $ "Empty nested 'use' detected. Impossible: This program certainly does not pass 'rustc'." <> show u
        join <$> mapM (extractImports path') nesteds'

      extractAlgo ::
        CompM m =>
        FnDecl Span ->
        Block Span ->
        m (FrLang.Expr RustArgType)
      extractAlgo (FnDecl args _ _ _) block = do
        args' <- mapM ((`evalStateT` HM.empty) . convertPat <=< SubC.convertArg) args
        block' <- convertIntoFrExpr block
        return $ LamE args' block'

      convertIntoFrExpr :: CompM m => Block Span -> m (FrLang.Expr RustArgType)
      convertIntoFrExpr rustBlock = do
        subsetExpr <- SubC.convertBlock rustBlock
        evalStateT (convertExpr subsetExpr) HM.empty

      toBindings p@(Path _ segments _) =
        forM segments $ \case
          (PathSegment ident Nothing _) -> return $ toBinding ident
          (PathSegment _ (Just _) _) -> throwError $ "We currently do not support import paths with path parameters.\n" <> show p

  loadTypes ::
    CompM m =>
    Language 'Rust ->
    Module ->
    Namespace (FrLang.Expr RustArgType) (Item Span) ->
    m (Namespace (FrLang.Expr RustArgType) (Item Span))
  loadTypes _ (Module ownFile _) ohuaNs = do
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNs ^. algos)
    --traceShowM $ "files and paths: " <> show filesAndPaths
    let filesAndPaths' = map (first convertOwn) filesAndPaths
    typez <- load $ concatMap fst filesAndPaths'
    -- traceShowM $ "loaded types: " <> show types
    types' <- HM.fromList . catMaybes <$> mapM (verifyAndRegister typez) filesAndPaths'
    -- traceShowM $ "extracted types: " <> show types'
    updateExprs ohuaNs (transformM (assignTypes types'))
    where
      assignTypes :: CompM m => FunTypes -> FrLang.Expr RustArgType -> m (FrLang.Expr RustArgType)
      -- FIXME It is nice to write it like this, really, but this has to check whether the function used in the code has (at the very least)
      --       the same number of arguments as it is applied to. If this does not match then clearly we need to error here!
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

      funsForAlgo :: CompM m => Algo (FrLang.Expr RustArgType) (Item Span) -> m [([NSRef], QualifiedBinding)]
      funsForAlgo (Algo _name code _) = do
        -- traceShowM $ "algo: " <> show _name <> "\n code: \n" <> quickRender code
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

      load :: CompM m => [NSRef] -> m FunTypes
      load nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".rs")) nsRefs

      convertOwn :: [NSRef] -> [NSRef]
      convertOwn [] = [filePathToNsRef ownFile]
      convertOwn n = n

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

createFunTypeArgs :: ConvertM m => QualifiedBinding -> [Sub.Expr] -> m (Either Unit (NonEmpty (ArgType RustArgType)))
createFunTypeArgs ref args =
  case args of
    [] -> return $ Left Unit
    (x : xs) -> Right <$> mapM extractType (x :| xs)

extractType :: ConvertM m => Sub.Expr -> m (ArgType RustArgType)
extractType (Sub.Var bnd) =
  maybe TypeVar intoRustArgType . HM.lookup bnd <$> get
  where
    intoRustArgType (Sub.RustType ty) = Type $ TE.Normal ty
extractType _ = return TypeVar

instance ConvertExpr Sub.Expr where
  convertExpr (Sub.Call fun args) = do
    fun' <- convertExpr fun
    fun'' <- case fun' of
      LitE (FunRefLit (FunRef ref _ _)) -> do
        ty <- createFunTypeArgs ref args
        return $ LitE (FunRefLit (FunRef ref Nothing $ FunType ty))
      _ -> return fun'
    args' <- mapM convertExpr args
    return $ fun'' `AppE` args'
  convertExpr (Sub.MethodCall receiver (Sub.CallRef method _) args) = do
    receiver' <- convertExpr receiver
    argTys <- createFunTypeArgs method args
    receiverTy <- extractType receiver
    let method' = LitE (FunRefLit (FunRef method Nothing $ STFunType receiverTy argTys))
    args' <- mapM convertExpr args
    return $ BindE receiver' method' `AppE` args'
  convertExpr (Sub.Tuple vars) = do
    vars' <- mapM convertExpr vars
    return $ TupE vars'
  convertExpr (Sub.Binary op left right) = do
    op' <- convertExpr op
    left' <- convertExpr left
    right' <- convertExpr right
    return $ op' `AppE` [left', right']
  convertExpr (Sub.Unary op arg) = do
    op' <- convertExpr op
    arg' <- convertExpr arg
    return $ op' `AppE` [arg']
  convertExpr (Sub.Lit l) = convertExpr l
  convertExpr (Sub.If expr trueBlock falseBlock) = do
    expr' <- convertExpr expr
    trueBlock' <- convertExpr trueBlock
    falseBlock' <- maybe (return $ FrLang.LitE UnitLit) convertExpr falseBlock
    return $ IfE expr' trueBlock' falseBlock'
  convertExpr (Sub.While cond block) = do
    cond' <- convertExpr cond
    block' <- convertExpr block
    -- TODO proper name generation
    let loopLambdaRef = "while_loop_body"
    let recur =
          IfE
            cond'
            (VarE loopLambdaRef `AppE` [])
            $ LitE UnitLit
    return $
      -- FIXME proper name generation needed here!
      LetE
        (VarP loopLambdaRef)
        (LamE [] $ StmtE block' recur)
        recur
  convertExpr (Sub.ForLoop pat dataExpr body) = do
    pat' <- convertPat pat
    dataExpr' <- convertExpr dataExpr
    body' <- convertExpr body
    return $
      MapE
        (LamE [pat'] body')
        dataExpr'
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
    return $ LitE $ FunRefLit $ FunRef ref Nothing Untyped
  convertExpr (Sub.Var bnd) = return $ VarE bnd

instance ConvertExpr Sub.Block where
  convertExpr (Sub.RustBlock stmts Sub.Normal) =
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
      convertStmt :: ConvertM m => Sub.Stmt -> m (FrLang.Expr RustArgType -> FrLang.Expr RustArgType)
      convertStmt (Sub.Local pat ty e) = do
        case (pat, ty) of
          (Sub.IdentP (Sub.IdentPat _ bnd), Just ty') -> modify (HM.insert bnd ty')
          _ -> return ()
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

instance ConvertPat Sub.Pat where
  convertPat Sub.WildP = return $ VarP $ fromString "_"
  convertPat (Sub.IdentP ip) = convertPat ip
  convertPat (Sub.TupP patterns) = TupP <$> mapM convertPat patterns

instance ConvertPat Sub.IdentPat where
  convertPat (Sub.IdentPat mutability bnd) = return $ VarP bnd

instance ConvertPat Sub.Arg where
  convertPat (Sub.Arg pat ty) = do
    case pat of
      Sub.IdentP (Sub.IdentPat _ bnd) -> modify (HM.insert bnd ty)
      _ -> return ()
    convertPat pat

instance ConvertExpr Sub.BinOp where
  convertExpr Sub.Add = toExpr "+"
  convertExpr Sub.Sub = toExpr "-"
  convertExpr Sub.Mul = toExpr "*"
  convertExpr Sub.Div = toExpr "/"
  convertExpr Sub.Lt = toExpr "<"
  convertExpr Sub.Lte = toExpr "<="
  convertExpr Sub.Gte = toExpr ">="
  convertExpr Sub.Gt = toExpr ">"
  convertExpr Sub.EqOp = toExpr "=="

instance ConvertExpr Sub.UnOp where
  convertExpr Sub.Deref = toExpr "*"
  convertExpr Sub.Not = toExpr "!"
  convertExpr Sub.Neg = toExpr "-"

toExpr op = return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) op) Nothing Untyped

instance ConvertExpr Sub.Lit where
  convertExpr (Sub.Int i) = return $ LitE $ NumericLit i
  convertExpr (Sub.Bool b) = return $ LitE $ BoolLit b
