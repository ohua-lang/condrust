{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Ohua.Integration.Rust.Frontend where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.List (stripPrefix)

import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)

import Ohua.Prelude
import qualified Ohua.Prelude as Res (FunType)
import Ohua.Frontend.Types
import Ohua.Frontend.TypeSystem (Delta)
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Lang as FrLang
    ( Expr(..),
      Pat(TupP, VarP),
      flatten,
      patType)

import Ohua.Integration.Lang
import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.Types.Extraction as TH
import qualified Ohua.Integration.Rust.Frontend.Subset as Sub
import qualified Ohua.Integration.Rust.Frontend.Convert as SubC


class ConvertExpr a where
  convertExpr :: SubC.ConvertM m => a -> m (FrLang.Expr RustVarType Unresolved)

class ConvertPat a where
  convertPat :: SubC.ConvertM m => a -> m (FrLang.Pat RustVarType Unresolved)

instance Integration (Language 'Rust) where
  type HostModule (Language 'Rust) = Module
  type Type (Language 'Rust) = RustVarType
  type AlgoSrc (Language 'Rust) = Item Span

  loadNs :: forall m.
    ErrAndLogM m =>
    Language 'Rust ->
    FilePath ->
    m (Module, Namespace (FrLang.Expr RustVarType Unresolved) (Item Span), Module)
  loadNs _ srcFile = do
    rustMod <- liftIO $ loadRustFile srcFile
    rustNS <- extractNs rustMod
    return (Module srcFile rustMod, rustNS, Module "placeholderlib.rs" placeholderModule)
    where
      extractNs ::
        ErrAndLogM m =>
        SourceFile Span ->
        m (Namespace (FrLang.Expr RustVarType Unresolved) (Item Span))
      extractNs input = do
        globalDefs <- collectGlobals input
        extractWithGlobals globalDefs input
        where
          -- FIXME globals are in the wrong place here.
          --       they belong into the delta because their only purpose is to deliver the type for
          --       free vars in the algos
          collectGlobals (SourceFile _ _ items) = do
            return $
              foldl (\hm (k, v) -> HM.insert k v hm) HM.empty
                    $ mapMaybe
                    (\case
                        (ConstItem _attr _pub ident ty _expr _span )-> Just (toBinding ident, Sub.RustType (deSpan ty))
                        (Static _ _ ident ty Immutable _expr _span) -> Just (toBinding ident, Sub.RustType (deSpan ty))
                        (Static _ _ ident _ty Mutable _expr _span) ->
                          error $ "You are trying to use a global mutable object "<> show ident <>" in a concurrent program ... DON'T"
                        _ -> Nothing
                    )
                    items

      extractWithGlobals globs (SourceFile _ _ items) = do
        rustImports <-
          concat . catMaybes <$> mapM (\case
                                          (Use _ _ t _) -> Just . toList <$> extractImports [] t
                                          _             -> return Nothing) items
        rustAlgos <-
          catMaybes <$> mapM (\case
                                 f@(Fn _ _ ident decl _ _ block _) ->
                                   Just . (\e -> Algo (toBinding ident) e f) <$> extractAlgo ident decl block
                                 _ -> return Nothing) items
        return $ Namespace (filePathToNsRef srcFile) rustImports (map Global $ HM.keys globs) rustAlgos

      -- FIXME: Paths inside a crate would be use crate::something. Actually we'd need proper name resolution for this i.e. accoding to
      -- how cargo works. For now I'll assume crate imports to just live in the same directory, replacing 'crate' with './'
      extractImports :: ErrAndLogM m => [Binding] -> UseTree Span -> m (NonEmpty Import)
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
        (SubC.ConvertM m, ErrAndLogM m) =>
        Ident ->
        FnDecl Span ->
        Block Span ->
        m (FrLang.Expr RustVarType Unresolved)
      extractAlgo _algoName (FnDecl _ _ True _) _block = error "We do not support compilation of variadic functions for the moment"
      extractAlgo _algoName  (FnDecl args mReturnTy _ _) block = do
        args' <- mapM (convertPat <=< SubC.convertArg) args
        block' <- convertIntoFrExpr block
        let returnType = maybe UType asHostNormalU mReturnTy
        -- FIXME: We use dto do this as a transformation in the Frontend i.e. Transform.Env (prepareRootAlgoVars)
        --        I really think that this is the way we should do it instead of expecting the integrations to do this quirk for us.
        --        We need this transformation to have the return type of the algo inside the algo i.e. for type checking.
        let wrappedBlock = LetE (VarP "result" returnType) block' (VarE "result" returnType)
        return $ LamEU {- implGlobalArgs ++ -} args' wrappedBlock

      convertIntoFrExpr :: SubC.ConvertM m => Block Span -> m (FrLang.Expr RustVarType Unresolved)
      convertIntoFrExpr rustBlock = do
        subsetExpr <- SubC.convertBlock rustBlock
        convertExpr subsetExpr

      toBindings p@(Path _ segments _) =
        forM segments $ \case
          (PathSegment Ident{name=iname} Nothing _) -> if iname == "crate"
                                              then return $ fromString "."
                                              else return $ fromString iname
          (PathSegment _ (Just _) _) -> error $ "We currently do not support import paths with path parameters.\n" <> show p
{-
      fromGlobals :: SubC.ConvertM m => m [FrLang.Pat RustVarType]
      fromGlobals = do
        map (\(bnd, Sub.RustType ty) -> VarP bnd (asHostNormalU ty)) . HM.toList <$> get
-}

  loadTypes ::
    ErrAndLogM m =>
    Language 'Rust ->
    Module ->
    Namespace (FrLang.Expr RustVarType Unresolved) (Item Span) ->
    m (Delta RustVarType Resolved)
  loadTypes _ (Module ownFile _) ohuaNs = do
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNs ^. algos)
    -- To enable the type extraction to find local algorithms, we add the filepath to the current file as their namespace 
    let filesAndPaths' = map (first convertLocalToPath) filesAndPaths
    importedFnTypes <- fnTypesFromFiles $ concatMap fst filesAndPaths'
    -- We won't have (or don't want to need) the filepath later, when we typecheck the functions in delta
    -- Therefore we replace the namespace back to an empty space, so that we can find the local functions later
    -- in delta. Another option would be to add the local file path to the imported paths 
    let fixedLocalScope =  foldr (\(qb, ty) hm -> HM.insert (convertPathToLocal qb) ty hm) HM.empty (HM.toList importedFnTypes)
    return fixedLocalScope
    where
      funsForAlgo :: ErrAndLogM m => Algo (FrLang.Expr RustVarType Unresolved) (Item Span) -> m [([NSRef], QualifiedBinding)]
      funsForAlgo (Algo _name code _inputCode ) = do
          mapM
            lookupFunTypes
            $ [f                 | LitE (FunRefLit (FunRef f _ _)) <- flatten code] ++
              [toQualBinding bnd | VarE bnd _                      <- flatten code]

      convertLocalToPath :: [NSRef] -> [NSRef]
      convertLocalToPath [] = [filePathToNsRef ownFile]
      convertLocalToPath n = n

      convertPathToLocal :: QualifiedBinding -> QualifiedBinding
      convertPathToLocal qb@(QualifiedBinding (NSRef spaces) bnd) = 
        case stripPrefix (filePathToList ownFile) spaces of
                  Just localspaces -> QualifiedBinding (NSRef localspaces) bnd
                  Nothing -> {-local path wasn't a prefix-} qb

      -- | Extract a HashMap of {name: function type} from the given file reference
      fnTypesFromFiles :: ErrAndLogM m => [NSRef] -> m FunTypesMap
      fnTypesFromFiles nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".rs")) nsRefs

      -- | Deal with Globs here.
      {- 
      verifyAndRegister :: ErrAndLogM m
                        => FunTypesMap
                        -> ([NSRef], QualifiedBinding)
                        -> m (Maybe (QualifiedBinding, Res.FunType RustVarType Resolved))
      verifyAndRegister typez ([candidate], qp@(QualifiedBinding (NSRef nsRef) nam)) = do
        let ref = case nsRef of
                    [] -> QualifiedBinding candidate nam
                    _ -> qp
        case HM.lookup ref typez of
          Just t -> return $ Just (qp, t)
          Nothing -> do
            traceM $ "Function `" <> show ref <> "` not found in module `" <> show candidate <> "`."
            traceM $ "Extracted: \n" <> quickRender (HM.toList typez)
            return Nothing
      verifyAndRegister typez (globs', qp@(QualifiedBinding _ nam)) = do
        case mapMaybe ((`HM.lookup` typez) . (`QualifiedBinding` nam)) globs' of
          [] -> do
            traceM $ "Function `" <> show (unwrap nam) <> "` not found in modules `" <> show globs' <> "`. "
            return Nothing
          [t] -> return $ Just (qp, t)
          _ -> throwError $ "Multiple definitions of function `" <> show (unwrap nam) <> "` in modules " <> show globs' <> " detected!\nPlease verify again that your code compiles properly by running `rustc`. If the problem persists then please file a bug. (See issue sertel/ohua-frontend#1)"
      -}
      lookupFunTypes :: ErrAndLogM m => QualifiedBinding -> m ([NSRef], QualifiedBinding)
      lookupFunTypes q@(QualifiedBinding (NSRef []) nam) =
        return $ (,q) $ maybe globs (: []) $ HM.lookup nam fullyResolvedImports
      lookupFunTypes q@(QualifiedBinding nsRef _name) =
        -- FIXME: This doesn't consider empty NSRefs, if there should be none we need to make it NonEmpty
        let (aliaz : rest) = unwrap nsRef
         in case HM.lookup aliaz aliasImports of
              Just a -> return ([NSRef $ unwrap a ++ rest], q)
              Nothing -> case globs of
                [] ->
                  -- Question: Supposed we move the type  loading before the actual lowering, and only require a definite typing as we
                  --           lower the code (i.e. if we do not know the types during lowering we require a type annotation), 
                    --         should we go on here without an error? -> Spoiler: I think so
                  throwError $
                    "I found the module alias reference '"
                      <> show q
                      <> "' that is not listed in the imports and no glob imports (::*) were defined: '"
                      <> show globs
                      <> "'\n Please move it into this module if you happen to import this via a mod.rs file."
                xs -> return (xs, q)


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


-- ISSUE: I thought we could support Assignments as we actually just need to replace them with ssa let bindings.
--        However the compiler does constant propagation so a `mut i:i32 = 1` will be replaced by `1` 
--        before we reach the point were it is reassigned -> I need to check if/how this can work out

instance ConvertExpr Sub.Expr where
  convertExpr (Sub.Call fun args) = do
    fun' <- convertExpr fun
    args' <- mapM convertExpr args
    return $ fun' `AppEU` args'
  -- ToDo: I don't think we should pass the receiver just as a binding, losing potential type information and
  convertExpr (Sub.MethodCall objE (Sub.CallRef (QualifiedBinding nsRef funName) _) args) = do
    objE' <- convertExpr objE
    args' <- mapM convertExpr args
    return $ BindE objE' funName args'
  -- FIXME: This belongs into subset conversion
  convertExpr (Sub.MethodCall obj _ _ ) = error $ "A method call on something other than a struct variable, namely "<> show obj <>" was found but we only support variables."
  convertExpr (Sub.Tuple []) = return (LitE UnitLit)
  convertExpr (Sub.Tuple (v:vars)) = do
    v'<- convertExpr v
    vars' <- mapM convertExpr vars
    return $ TupE (v':|vars')
  convertExpr (Sub.Binary op left right) = do
    left' <- convertExpr left
    right' <- convertExpr right
    let (symbol, retTy) = binOpInfo op
    funref <- asFunRef symbol (TStar :| [TStar]) retTy
    return $  AppEU funref [left',right']
  convertExpr (Sub.Unary op arg) = do
    arg' <- convertExpr arg
    let (symbol, retTy) = unOpInfo op
    funref <- asFunRef symbol (TStar :| []) retTy
    return $ AppEU funref [arg']
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
  convertExpr (Sub.ForLoop pat dataExpr body) = do
    pat' <- convertPat pat
    dataExpr' <- convertExpr dataExpr
    body' <- convertExpr body
    return $
      MapE
        (LamEU [pat'] body')
        dataExpr'
  convertExpr (Sub.EndlessLoop body) = convertExpr $ Sub.While (Sub.Lit $ Sub.Bool True) body
  convertExpr (Sub.Closure _ _ _ args _retTy body) = do
    args' <- mapM convertPat args
    body' <- convertExpr body
    return $ LamEU args' body'
  convertExpr (Sub.BlockExpr block) = convertExpr block
  convertExpr (Sub.PathExpr (Sub.CallRef ref _tyInfo)) = do
    return $ LitE $ FunRefLit $ FunRef ref Nothing $ FunType (TStar :| []) TStar
  convertExpr (Sub.Var bnd (Just (Sub.RustType ty))) = return $ VarE bnd (asHostNormalU ty)
  convertExpr (Sub.Var bnd Nothing) = return $ VarE bnd TStar

instance ConvertExpr Sub.Block where
  convertExpr (Sub.RustBlock Sub.Normal stmts) =
    convertStmts stmts
    where
      convertStmts [] = return $ LitE UnitLit
      convertStmts (x : xs) =
        let lastS = NE.last (x :| xs)
            heads = NE.init (x :| xs)
         in do
              convertedHeads <- mapM convertStmt heads
              convertedLast <- convertLastStmt lastS
              return $
                foldr
                  (\stmt cont -> stmt cont)
                  convertedLast
                  convertedHeads
      convertStmt :: SubC.ConvertM m => Sub.Stmt -> m (FrLang.Expr RustVarType Unresolved -> FrLang.Expr RustVarType Unresolved)
      convertStmt (Sub.Local pat _ty e) = do
        {-
        case (pat, ty) of
          -- ToDo: We should move this to Rust -> Sub Conversion and set it automatically if the RHS is a literal
          (Sub.IdentP (Sub.IdentPat _mode bnd), Just ty') -> modify (HM.insert bnd ty')
          (Sub.TupP idents, Just (Sub.RustType (TupTy tys _))) -> do
            let asSubRustType = map Sub.RustType tys
                asBindings = map fromIdent idents
                keysAndValues = zip asBindings asSubRustType
            modify (\context-> foldl (\c (a,b) ->  HM.insert a b c) context keysAndValues)
          untypedpattern -> return () --ToDo: Should we warn here? logWarnN $ "Targets of the assignment: " <> show s <> " are not annotated\n"
        -}
        pat' <- convertPat pat
        e' <- convertExpr e
        return $ LetE pat' e'
      convertStmt (Sub.NoSemi e) = StmtE <$> convertExpr e
      convertStmt (Sub.Semi e) = StmtE <$> convertExpr e
      convertStmt Sub.StandaloneSemi = return id

      convertLastStmt e@(Sub.NoSemi expr) =
        case expr of
          Sub.ForLoop {} -> (\e' -> e' $ LitE UnitLit) <$> convertStmt e
          _ -> convertExpr expr
      convertLastStmt e = (\e' -> e' $ LitE UnitLit) <$> convertStmt e

fromIdent :: Sub.IdentPat -> Binding
fromIdent (Sub.IdentPat _mode bnd) =  bnd

instance ConvertPat Sub.Pat where
  convertPat Sub.WildP   = return $ VarP (fromString "_") (asHostNormalU rustInfer)
  convertPat (Sub.IdentP ip) = convertPat ip
  -- It is possible to bind: let () = e1; in Rust, iff e1 evaluates to ()
  -- The pattern in this case would be TupP [], because language-rust has no Unit pattern
  -- However we do so we distinguish cases here
  convertPat (Sub.TupP []) = throwError "Error: empty tuple pattern detected. This is currently not supported."
  convertPat (Sub.TupP (pt : pts)) = do
    pt' <- convertPat pt
    pts' <- mapM convertPat pts
    return $ TupP (pt' :| pts')

instance ConvertPat Sub.IdentPat where
  convertPat (Sub.IdentPat _mutability bnd) = do
    return $ VarP bnd TStar

instance ConvertPat Sub.Arg where
  convertPat (Sub.Arg pat (Sub.RustType rustTy)) =  do
    pat' <- convertPat pat
    case pat' of
      VarP bnd TStar -> return $ VarP bnd (HType (HostType (TH.Normal rustTy)) Nothing)
      -- FIXME: We need to pass on the type to tuples as well I assume
      --        The most critical part are varaibes though, because we use this function to convert 
      --        the algo argumets, and we need their types obviously
      p -> return p


binOpInfo :: Sub.BinOp -> (Binding, OhuaType RustVarType Unresolved)
-- ToDo: It seems infering the type of binary ops should be easy given the input. However
--       we might need to incorparate Rusts coercion rules if the two arguments differ in type
  -- Check if that applicable and if it would be a problem after all since when somethigncan be coerced for this binary op, it can also be coerced downstream probably, if we just take one of the types.
binOpInfo Sub.Add =  ("+", TStar)
binOpInfo Sub.Sub =  ("-", TStar)
binOpInfo Sub.Mul =  ("*", TStar)
binOpInfo Sub.Div =  ("/", TStar)
binOpInfo Sub.Lt =  ("<", TStar)
binOpInfo Sub.Lte =  ("<=", asHostNormalU rustBool)
binOpInfo Sub.Gte =  (">=", asHostNormalU rustBool)
binOpInfo Sub.Gt =  (">", asHostNormalU rustBool)
binOpInfo Sub.EqOp =  ("==", asHostNormalU rustBool)
binOpInfo Sub.OrOp =  ("||", asHostNormalU rustBool)

unOpInfo :: Sub.UnOp -> (Binding, OhuaType RustVarType Unresolved)
unOpInfo Sub.Deref =  ("*",  TStar)
-- ToDo: negation is a trait and returns whatever the input type implemented it to return i.e. not necessarily of the same type
unOpInfo Sub.Not =  ("!",  TStar)
unOpInfo Sub.Neg =  ("-",  TStar)

asFunRef :: Monad m => Binding -> NonEmpty (OhuaType RustVarType Unresolved) -> OhuaType RustVarType Unresolved -> m (FrLang.Expr RustVarType Unresolved)
asFunRef op tys retTy = return $
      LitE $ FunRefLit $
        FunRef (QualifiedBinding (makeThrow []) op) Nothing (FunType tys retTy)


toQualBinding :: Binding -> QualifiedBinding
toQualBinding = QualifiedBinding (makeThrow [])

instance ConvertExpr Sub.Lit where
  convertExpr (Sub.Int i) = return $ LitE $ NumericLit i
  convertExpr (Sub.Bool b) = return $ LitE $ BoolLit b
