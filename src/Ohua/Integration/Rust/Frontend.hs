{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Integration.Rust.Frontend where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Frontend.Convert
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Types
import Ohua.Integration.Lang
import Ohua.Integration.Rust.TypeExtraction
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Prelude
import qualified Ohua.Integration.Rust.Frontend.Convert as SubC
import qualified Ohua.Integration.Rust.Frontend.Subset as Sub


instance Integration (Language 'Rust) where
  type NS (Language 'Rust) = Module
  type Type (Language 'Rust) = RustArgType Span
  type AlgoSrc (Language 'Rust) = Item Span

  loadNs ::
    CompM m =>
    Language 'Rust ->
    FilePath ->
    m (Module, Namespace (FrLang.Expr (RustArgType Span)) (Item Span))
  loadNs _ srcFile = do
    mod <- liftIO $ load srcFile
    ns <- extractNs mod
    return (Module srcFile mod, ns)
    where
      extractNs ::
        CompM m =>
        SourceFile Span ->
        m (Namespace (FrLang.Expr (RustArgType Span)) (Item Span))
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
        m (FrLang.Expr (RustArgType Span))
      extractAlgo (FnDecl args _ _ _) block = do
        args' <- mapM (convertPat <=< SubC.convertArg) args
        block' <- convertIntoFrExpr block
        return $ LamE args' block'

      convertIntoFrExpr :: CompM m => Block Span -> m (FrLang.Expr (RustArgType Span))
      convertIntoFrExpr rustBlock = do
        subsetExpr <- SubC.convertBlock rustBlock
        -- TODO propagate the types of the variables as context
        frExpr <- convertExpr subsetExpr
        return frExpr

      toBindings p@(Path _ segments _) =
        forM segments $ \case
          (PathSegment ident Nothing _) -> return $ toBinding ident
          (PathSegment _ (Just _) _) -> throwError $ "We currently do not support import paths with path parameters.\n" <> show p

  loadTypes ::
    CompM m =>
    Language 'Rust ->
    Module ->
    Namespace (FrLang.Expr (RustArgType Span)) (Item Span) ->
    m (Namespace (FrLang.Expr (RustArgType Span)) (Item Span))
  loadTypes _ (Module ownFile _) ohuaNs = do
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNs ^. algos)
    --traceShowM $ "files and paths: " <> show filesAndPaths
    let filesAndPaths' = map (first convertOwn) filesAndPaths
    typez <- load $ concatMap fst filesAndPaths'
    -- traceShowM $ "loaded types: " <> show types
    types' <- HM.fromList <$> mapM (verifyAndRegister typez) filesAndPaths'
    -- traceShowM $ "extracted types: " <> show types'
    updateExprs ohuaNs (transformM (assignTypes types'))
    where
      assignTypes :: CompM m => FunTypes -> FrLang.Expr (RustArgType Span) -> m (FrLang.Expr (RustArgType Span))
      -- FIXME It is nice to write it like this, really, but this has to check whether the function used in the code has (at the very least)
      --       the same number of arguments as it is applied to. If this does not match then clearly we need to error here!
      assignTypes typez = \case
        (LitE (FunRefLit (FunRef qb i _))) ->
          case HM.lookup qb typez of
            Just typ ->
              -- do
              --     traceShowM $ "Fun: " <> show qb <> " Type: " <> show typ
              return $ LitE $ FunRefLit $ FunRef qb i typ
            Nothing -> throwError "Compiler invariant broken."
        e -> return e

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

      funsForAlgo :: CompM m => Algo (FrLang.Expr (RustArgType Span)) (Item Span) -> m [([NSRef], QualifiedBinding)]
      funsForAlgo (Algo _name code _) = do
        -- traceShowM $ "algo: " <> show _name <> "\n code: \n" <> quickRender code
        mapM lookupFunTypes [f | LitE (FunRefLit (FunRef f _ _)) <- universe code]

      lookupFunTypes :: CompM m => QualifiedBinding -> m ([NSRef], QualifiedBinding)
      lookupFunTypes q@(QualifiedBinding [] nam) =
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

      verifyAndRegister :: CompM m => FunTypes -> ([NSRef], QualifiedBinding) -> m (QualifiedBinding, FunType (RustArgType Span))
      verifyAndRegister typez ([candidate], qp@(QualifiedBinding _ nam)) =
        case HM.lookup (QualifiedBinding candidate nam) typez of
          Just t -> return (qp, t)
          Nothing -> throwError $ "Function `" <> show (unwrap nam) <> "` not found in module `" <> show candidate <> "`. I need these types to generate the code. Please check that the function actually exists by compiling again with `rustc`."
      verifyAndRegister typez (globs', qp@(QualifiedBinding _ nam)) =
        case mapMaybe ((`HM.lookup` typez) . (`QualifiedBinding` nam)) globs' of
          [] -> throwError $ "Function `" <> show (unwrap nam) <> "` not found in modules `" <> show globs' <> "`. I need these types to generate the code. Please check that the function actually exists by compiling again with `rustc`."
          [t] -> return (qp, t)
          _ -> throwError $ "Multiple definitions of function `" <> show (unwrap nam) <> "` in modules " <> show globs' <> " detected!\nPlease verify again that your code compiles properly by running `rustc`. If the problem persists then please file a bug. (See issue sertel/ohua-frontend#1)"

instance ConvertExpr Sub.Expr where
  convertExpr (Sub.Call fun args) = do
    fun' <- convertExpr fun
    args' <- mapM convertExpr args
    return $ fun' `AppE` args'
  convertExpr (Sub.MethodCall receiver method args) = do
    receiver' <- convertExpr receiver
    method' <- convertExpr $ Sub.PathExpr method
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
  convertExpr (Sub.Closure _ _ _ args retTy body) = do
    -- TODO handle type information
    args' <- mapM convertPat args
    body' <- convertExpr body
    return $ LamE args' body'
  convertExpr (Sub.BlockExpr block) = convertExpr block
  convertExpr (Sub.PathExpr (Sub.CallRef ref tyInfo)) =
    -- TODO handle type info
    return $ LitE $ FunRefLit $ FunRef ref Nothing Untyped
  convertExpr (Sub.Var bnd) = return $ VarE bnd

-- instance (Show a) => ConvertExpr (Sub.Path) where
--   -- This needs context information to distinguish a path from a variable.
--   -- A transformation is performing this disambiguation later on.
--   convertExpr (Path _ segments _) =
--     case segments of
--       [segment] -> VarE . fromString <$> convertSegment segment
--       segs -> do
--         segments' <- mapM convertSegment segments
--         let (x : revPath) = reverse segments'
--         return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow $ map fromString $ reverse revPath) $ fromString x) Nothing Untyped
--     where
--       convertSegment (PathSegment Ident {name = n} Nothing _) = return n
--       convertSegment e@PathSegment {} = throwError $ "Currently, we do not support type parameters in paths.\n" <> show e

instance ConvertExpr Sub.Block where
  convertExpr (Sub.RustBlock stmts Sub.Normal) =
    case stmts of
      [] -> return $ LitE UnitLit
      (x : xs) ->
        let last = NE.head $ NE.reverse $ x :| xs
            heads = NE.tail $ NE.reverse $ x :| xs
         in do
              convertedLast <- convertLastStmt last
              foldM
                (\cont stmt -> (\e -> e cont) <$> convertStmt stmt)
                convertedLast
                heads
    where
      convertStmt :: CompM m => Sub.Stmt -> m (FrLang.Expr ty -> FrLang.Expr ty)
      convertStmt (Sub.Local pat e) = do
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
  -- TODO capture type information here! (with respect to mutability)
  convertPat (Sub.IdentPat mutability bnd) = return $ VarP bnd

instance ConvertPat Sub.Arg where
  -- TODO here we need to capture the type information
  convertPat (Sub.Arg pat ty) = convertPat pat

instance ConvertExpr Sub.BinOp where
  convertExpr Sub.Add  = toExpr "+"
  convertExpr Sub.Sub  = toExpr "-"
  convertExpr Sub.Mul  = toExpr "*"
  convertExpr Sub.Div  = toExpr "/"
  convertExpr Sub.Lt   = toExpr "<"
  convertExpr Sub.Lte  = toExpr "<="
  convertExpr Sub.Gte  = toExpr ">="
  convertExpr Sub.Gt   = toExpr ">"
  convertExpr Sub.EqOp = toExpr "=="

instance ConvertExpr Sub.UnOp where
  convertExpr Sub.Deref = toExpr "*"
  convertExpr Sub.Not   = toExpr "!"
  convertExpr Sub.Neg   = toExpr "-"

toExpr op = return $ LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) op) Nothing Untyped

instance ConvertExpr Sub.Lit where
  convertExpr (Sub.Int i) = return $ LitE $ NumericLit i
  convertExpr (Sub.Bool b) = return $ LitE $ BoolLit b
