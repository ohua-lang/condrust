{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}


module Ohua.Integration.Rust.Frontend where

import Data.Text.Lazy.IO as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)


import Ohua.Prelude hiding (getVarType)
import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Lang as FrLang
    ( Expr(..),
      Pat(TupP, VarP, WildP),
      exprType,
      patType,
      patBnd,
      setPatType,
      applyToFinal)

import Ohua.Integration.Lang
import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.TypeExtraction as TE
import qualified Ohua.Integration.Rust.Frontend.Subset as Sub
import qualified Ohua.Integration.Rust.Frontend.Convert as SubC




import qualified Control.Applicative as Hm
import GHC.Exts (the)

type VarTypeContext = HM.HashMap Binding (VarType RustVarType)
type TypeContextM m = (Monad m, MonadState VarTypeContext m)

currentReturnBnd::Binding
currentReturnBnd = "InternalCurrentReturn"

class ConvertExpr a where
  convertExpr :: SubC.ConvertM m => a -> m (FrLang.Expr RustVarType)

class ConvertPat a where
  convertPat :: SubC.ConvertM m => a -> m (FrLang.Pat RustVarType)

instance Integration (Language 'Rust) where
  type HostModule (Language 'Rust) = Module
  type Type (Language 'Rust) = RustVarType
  type AlgoSrc (Language 'Rust) = Item Span

  loadNs ::
    ErrAndLogM m =>
    Language 'Rust ->
    FilePath ->
    m (Module, Namespace (FrLang.Expr RustVarType) (Item Span), Module)

  loadNs _ srcFile = do
    mod <- liftIO $ loadRustFile srcFile
    ns <- extractNs mod
    return (Module srcFile mod, ns, Module "placeholderlib.rs" placeholderModule)
    where
      extractNs ::
        ErrAndLogM m =>
        SourceFile Span ->
        m (Namespace (FrLang.Expr RustVarType) (Item Span))

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
                        Just . (\e -> Algo (toBinding ident) e f) <$> evalStateT (extractAlgo decl block) globs
                      _ -> return Nothing
                  )
                  items
            return $ Namespace (filePathToNsRef srcFile) imports (map Global $ HM.keys globs) algos

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
        SubC.ConvertM m =>
        FnDecl Span ->
        Block Span ->
        m (FrLang.Expr RustVarType)
      extractAlgo (FnDecl args _ True _) block = error "We do not support compilation of variadic functions for the moment"
      extractAlgo (FnDecl args mReturnTy _ _) block = do
        -- globally defined values are (implicit) arguments to each function
        -- we get them from the context BEFORE parsing the algo and make them explicit argumets here. 
        -- BUT we need to treat them differently in non-shared-memory backends. This can be acchieved by also 
        --     saving them in an external structure (another field in the Algo type) but it would be better to modify the 
        --     Language IRs to reflect the 'environment' other than the arguments of compiled algorithms
        implGlobalArgs <- fromGlobals
        -- traceM $ "Implicit Arguments:" <> show implGlobalArgs
        -- Add the parameters and their types to the context
        args' <- mapM (convertPat <=< SubC.convertArg) args
        -- Convert the function block
        block' <- convertIntoFrExpr block
        let block'' = typeReturnExpr mReturnTy block'
        return $ LamE (implGlobalArgs ++ args') block''

      convertIntoFrExpr :: SubC.ConvertM m => Block Span -> m (FrLang.Expr RustVarType)
      convertIntoFrExpr rustBlock = do
        subsetExpr <- SubC.convertBlock rustBlock
        convertExpr subsetExpr

      toBindings p@(Path _ segments _) =
        forM segments $ \case
          (PathSegment Ident{name=iname} Nothing _) -> if iname == "crate"
                                              then return $ fromString "."
                                              else return $ fromString iname
          (PathSegment _ (Just _) _) -> error $ "We currently do not support import paths with path parameters.\n" <> show p

      getReturn :: FnDecl Span -> RustVarType
      -- FIXME: We may need more elaborate patterns for the type here
      -- Currently we only care for pure functions as algos and do nto support tuple
      -- returns (or any sort of elaborate types in the composition). If this changes, we need to 
      -- make the return type more specific
      getReturn (FnDecl _ (Just ty) _ _ ) = TE.Normal $ deSpan ty
      getReturn (FnDecl _ Nothing _ _ ) = TE.Normal TE.rustUnitReturn

      fromGlobals:: SubC.ConvertM m => m [FrLang.Pat RustVarType]
      fromGlobals = do
        map (\(bnd, Sub.RustType ty) -> VarP bnd (asHostNormal ty)) . HM.toList <$> get

      typeReturnExpr :: Maybe (Ty a) -> FrLang.Expr RustVarType -> FrLang.Expr RustVarType
      typeReturnExpr mReturnTy block =
         let varType = asVarType mReturnTy
             retyped = FrLang.applyToFinal (replaceType varType) block
         in retyped

         where asVarType Nothing = TypeUnit
               asVarType (Just ty) = asHostNormal ty

               replaceType :: VarType RustVarType -> FrLang.Expr RustVarType -> FrLang.Expr RustVarType
               replaceType vt e = case e of
                  VarE bnd _ty -> VarE bnd vt
                  LitE (EnvRefLit b _ty) -> LitE (EnvRefLit b vt)
                  LitE (FunRefLit (FunRef q i fty)) -> LitE (FunRefLit (FunRef q i  (setReturnType vt fty)))
                  lit@(LitE l) -> if getLitType l == vt
                                    then error $ "Return type of function " <> show vt <> " doesn't match type "<> show (getLitType l) <> " of returned literal"
                                    else lit
                  -- ToDo: We can also get TupE here, which needs to be handled

  loadTypes ::
    ErrAndLogM m =>
    Language 'Rust ->
    Module ->
    Namespace (FrLang.Expr RustVarType) (Item Span) ->
    m (Namespace (FrLang.Expr RustVarType) (Item Span))

  loadTypes _ (Module ownFile _) ohuaNs = do
    -- Extract namespace reference (filepath) and Qualified name for all functions called in each algo
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNs ^. algos)
    -- traceShowM $ "files and paths: " <> show filesAndPaths
    -- For functions from the compiled module (not imported), set the file path to the current file
    let filesAndPaths' = map (first convertOwn) filesAndPaths
    --  traceShowM $ "After convertOwn : " <> show filesAndPaths
    -- Go through all namespace references, parse the files and return all function types defined in there
    functionTypes <- fnTypesFromFiles $ concatMap fst filesAndPaths'
    -- traceShowM $ "Loaded function types: " <> show functionTypes
    -- For each function reference check the extracted function type, if function is not found or defined multiple
    -- times this will fail
    usedFunctionTypes <- HM.fromList . catMaybes <$> mapM (verifyAndRegister functionTypes) filesAndPaths'
    -- traceShowM $ "extracted types: " <> show usedFunctionTypes
    -- Replace type annotations of function literals with extracted types
    ohuaNsTypedFuns  <- updateExprs ohuaNs (transformM (assignTypes usedFunctionTypes))
    -- Types might now come form either annotations
    -- or the function types we just extracted, so we didn't error if something remains of type 'Unknown'.
    -- The types from annotations have allready been passed top-down through the code when we lowered it to a FrLang expression.
    -- Now the function literals have been typed (as far as possible).
    -- So now we go bootom-up and try to type annotate untyped varaibles by first trying to type the arguments of each function call using 
    -- information from the funciton literals and second try to type those arguments where they are assigned (i.e. further up in the code)
    -- using the type info gathered from their usage. 
    updateExprs ohuaNsTypedFuns finalTyping

    where
      funsForAlgo :: ErrAndLogM m => Algo (FrLang.Expr RustVarType) (Item Span) -> m [([NSRef], QualifiedBinding)]
      funsForAlgo (Algo _name code _inputCode ) = do
        -- traceShowM $ "algo: " <> show _name <> "\n code: \n" <> quickRender code
        -- ToDo: Show types of functions here. They should allready be correct after transformmation to IR
        mapM lookupFunTypes [f | LitE (FunRefLit (FunRef f _ _)) <- universe code]


      convertOwn :: [NSRef] -> [NSRef]
      convertOwn [] = [filePathToNsRef ownFile]
      convertOwn n = n

      -- | Extract a HashMap of {name: function type} from the given file reference
      fnTypesFromFiles :: ErrAndLogM m => [NSRef] -> m FunTypes
      fnTypesFromFiles nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".rs")) nsRefs


      verifyAndRegister :: ErrAndLogM m => FunTypes -> ([NSRef], QualifiedBinding) -> m (Maybe (QualifiedBinding, FunType RustVarType))
      verifyAndRegister typez ([candidate], qp@(QualifiedBinding _ nam)) =
        case HM.lookup (QualifiedBinding candidate nam) typez of
          Just t -> trace ("Verified type of " <> show nam <> " is "<> show t <> "\nI considere it fully typed: "<> show (fullyTyped t)) return $ Just (qp, t)
          Nothing -> do
            -- $ (logWarn) $ "Function `" <> show (unwrap nam) <> "` not found in module `" <> show candidate <> "`."
            return Nothing
      verifyAndRegister typez (globs', qp@(QualifiedBinding _ nam)) =
        case mapMaybe ((`HM.lookup` typez) . (`QualifiedBinding` nam)) globs' of
          [] -> do
            -- $ (logWarn) $ "Function `" <> show (unwrap nam) <> "` not found in modules `" <> show globs' <> "`. "
            return Nothing
          [t] -> return $ Just (qp, t)
          _ -> throwError $ "Multiple definitions of function `" <> show (unwrap nam) <> "` in modules " <> show globs' <> " detected!\nPlease verify again that your code compiles properly by running `rustc`. If the problem persists then please file a bug. (See issue sertel/ohua-frontend#1)"

      assignTypes :: ErrAndLogM m => FunTypes -> FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
      assignTypes typez = \case
        f@(LitE (FunRefLit (FunRef qb i ft))) | fullyTyped ft -> trace ("fully typed function: " <> show qb <> " Type is: " <> show ft) return f
        f@(LitE (FunRefLit (FunRef qb i ft))) ->
          case HM.lookup qb typez of
            Just typ ->
              do
                traceShowM $ "Typing Function : " <> show qb <> " with type " <> show typ
                return $ LitE $ FunRefLit $ FunRef qb i typ
            Nothing -> trace ("No lib function found for: " <> show qb) return f -- throwError $ "I was unable to determine the types for the call to '" <> show qb <> "'. Please provide type annotations."
        e -> return e

      -- | We go through expressions bottom-up and collect types of variables. Bottom-up implies, that we encounter the usage of a variable, bevor it's assignment. 
      --   Therefor we can use function argument types, extracted in the step before to annotate the argument variables at their usage site and via the context also in
      --   the expression in wich they are bound. There can be variables e.g. the return variable of a function, that are not used as an argument. We can type them also, 
      --   by tracing the current 'innermost' return type. 
      -- ToDo: Monadify
      finalTyping :: ErrAndLogM m => FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
      finalTyping expr =
            evalStateT (traversal expr) HM.empty
            -- State s m a -> s -> m a 

      traversal :: (ErrAndLogM m, TypeContextM m) =>  FrLang.Expr RustVarType -> m (FrLang.Expr RustVarType)
      traversal =
         \case
            l@(LetE pat e1 e2) -> do
              traceM $ "Working on let: " <> show l
              let realType = realPatType pat
              case realType of
                Just ty -> do
                      -- If pat was annotated, (potential) usages of pat and the corresponding function type arguments are annotated. 
                      -- The return type of e1 might not be annotated, this includes the return type of branches
                      -- > In this case make pat the current return type, proceed with e1 and afterwards with e2
                      ctxt <- get
                      modify (HM.insert currentReturnBnd (FrLang.patType pat))
                      e1' <- traversal e1
                      -- remove the current return again
                      put ctxt
                      e2' <- traversal e2
                      return $ LetE pat e1' e2'
                Nothing -> do
                  -- If pat was not annotated, we might get it's type from 
                  --  a)  the return type of e1 or
                  --  b)  usages in e2 (when we concluded argument types from function annotation) written to the context
                  -- For any let x = something in cont we want to fail if we don't have a return type for something
                  -- so in this case we need to first do e2 and check if pat was used i.e. we can draw the type from the context
                  -- set pats type as the current return type if it is known now
                  -- then process e1 and fail if we neither have a return type annotated nor a real Type as currentReturn
                  -- if we didn't fail, e1 was return type annotated and we can update pats type
                      traceM $ "Pattern is untyped" <> show pat
                      e2' <- traversal e2
                      ctxt <- get
                      traceM $ "Processed continuation. Context is " <> show ctxt
                      let pat' = tryUpdate ctxt pat
                      modify (HM.insert currentReturnBnd (FrLang.patType pat'))
                      e1' <- traversal e1
                      put ctxt -- remove the currentReturn again 
                      -- The last step failed, if pat was Unknown AND e1 had no return type so we might need to update pat's type now
                      -- as we allready typed e2 and as the outer expression doesn't know pat, we do not need to add it to the context
                      let pat'' = tryUpdate ctxt pat
                      return $ LetE pat'' e1' e2'

            AppE fun args -> do
              traceM $ "Working on app " <> show fun
              return $  AppE fun args -- ToDo: Check if args are typed. 
                                           --       If not, try to type them using the inputType of the 'fun' expression. This can be done by mapping finalTyping over input types and arguments, such that for each 
                                           --       argument the expected input type is the current return type. If this fails -> fail, otherwise currentRetType = exprType fun
            LamE pats body -> do
              traceM $ "Working on lambda" <> show pats
              return $  LamE pats body -- ToDo: The pats are used in the body, so proceed with the body filling the context and (if necessary) update pats typing or fail if that doesn't work, New currentRetType = exprType body
            IfE cond eTrue eFalse -> return $  IfE cond eTrue eFalse -- ToDo: 1. proceed with cond, return Type should be Rusts bool, 2. proceed with eTrue, 3. proceed with eFalse 4. assert equal branch return types 5. currentRetType = exprType eTrue
            WhileE e1 e2 -> return $  WhileE e1 e2 -- Nothing to do, we currently don't use it 
            MapE fun generator -> return $  MapE fun generator-- ToDo: MapE maps a function to a generator, so 1. proceed with the generator 
                                             --       2. the return type of the generator is the input type of the function. This way we can type the patterns bound in for loops, even if they are not used in the loop 
            BindE state method -> return $  BindE state method -- ToDo: 1. This is a usage of the state so check if it is typed 
                                                     --          (problem with states is, that while they are arguments to methods, they apears only with Self type so we can currently only type them upon assigment. 
                                                     --          We might do better in the TypeExtraction on impl itemy by carrying the type for which a method is implemented through)
                                                     --       2. Process the method (it's an AppE expression) 
                                                     --       3. currentRetType = exprType method
            StmtE e1 e2  -> return $  StmtE e1 e2 -- ToDo: This just 'does' e1 ignores the return value and procceds with e2. Hence things from e1 might be used in e2 so 
                                        --       1. proceed with e2
                                        --       2. proceed with e1
                                        --       3. currentRetTy = exprype e2
            SeqE e1 e2 -> return $ SeqE e1 e2 -- ToDo: Process e2 than process e1
            TupE funTy args -> return $ TupE funTy args -- ToDo: We use this to represent a tuple in rust and generate the funTy from the args i.e. the funTy won't help us typing the args. 
                                               --       Also, this whole expression is the usage of a tuple, not its assignment, so the current return type should be a tuple and we should be able to use it to type the args.
                                               --       1. proceed trying to type the args if their not typed -> fail if that fails
                                               --  
            l@(LitE lit) -> do
              traceM $ "Working on Literal: " <> show l
              case FrLang.exprType l of
                Type x ->  case x of
                    HostType TE.Unknown -> return $ LitE lit --throwError $ "Couldn't type literal " <> show lit <> "Found type"<> show x <>". Please help me out by providing annotations or report error"
                    _ -> return $ LitE lit
                _ -> return $ LitE lit
            e@(VarE bnd ty) -> do
              -- Either current return or the variable must be typed if so -> take type, otherwise -> fail 
              traceM $ "Working on Var: " <> show e
              ctxt <- get
              if isUnknown ty
                then
                  case HM.lookup currentReturnBnd ctxt of
                    Nothing -> throwError $ "Couldn't type " <> show bnd
                    Just ty' | isUnknown ty' ->  throwError $ "Couldn't type " <> show bnd
                    Just ty' -> return $ VarE bnd ty' -- That should be the return variable of the algo because we treat all other variables in the context of their expression (binding/or usage)
                else
                  do
                    traceM $ "Type is known, will insert binding to context"
                    modify (HM.insert bnd ty)
                    modify (HM.insert currentReturnBnd ty)
                    return e

      tryUpdate::  VarTypeContext -> FrLang.Pat RustVarType -> FrLang.Pat RustVarType
      -- We want to update the pattern bound on a LHS of a Let expression
      -- The two possible sources of type information are a) usage sites, in which case the name of the binding should be in the context 
      -- and b) the return type of the RHS in which case the currentReturn type should be avaiblable and not of type Unknown 
      tryUpdate ctxt = \case
        (FrLang.VarP bnd (Type (HostType Unknown))) -> do
            let mReturnType = HM.lookup currentReturnBnd ctxt
                mUsedType = HM.lookup bnd ctxt
            case (mReturnType, mUsedType) of
              (Just rty, _ ) | not (isUnknown rty) -> VarP bnd rty
              (_ , Just uty) | not (isUnknown uty) -> VarP bnd uty
              _  -> error $ "Couldn't type binding " <> show bnd
        tp@(FrLang.TupP pats) | (isNothing (realPatType tp)) -> do
                let mReturnType = HM.lookup currentReturnBnd ctxt -- Current return type could be a tuple type
                    mUsedTypes = mapM (>>= (`HM.lookup` ctxt)) (FrLang.patBnd tp)
                case (mReturnType, mUsedTypes) of
                  (Just rty, _ ) | not (isUnknown rty) -> error "We haven't implemented destruction of Rust tuple types as function return types. Please remind us to fix this." --return (VarP bnd rty)
                  (_, Just (t:ts)) -> setPatType (TupleTy (t:|ts)) tp
                  _  -> error $ "Couldn't type tuple binding " <> show tp
        vTyped@FrLang.VarP{} -> vTyped
        wp@FrLang.WildP{} -> wp  -- There's no value in typing a wild pattern becaus ewe type right to left here and wp's aren't used downstream


      lookupFunTypes :: ErrAndLogM m => QualifiedBinding -> m ([NSRef], QualifiedBinding)
      lookupFunTypes q@(QualifiedBinding (NSRef []) nam) =
        return $ (,q) $ maybe globs (: []) $ HM.lookup nam fullyResolvedImports
      lookupFunTypes q@(QualifiedBinding nsRef _name) =
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


      -- Question: Can we have functions as arguments? 
      fullyTyped :: FunType RustVarType -> Bool
      fullyTyped (FunType args retTy) = all (\case  (Type (HostType TE.Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (retTy: args)
      fullyTyped (STFunType sTy argTys retTy) = all (\case (Type (HostType TE.Unknown)) -> False; TypeFunction fty -> fullyTyped fty; _ -> True) (sTy: retTy: argTys)

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




argTypesFromContext :: SubC.ConvertM m => [Sub.Expr] -> m [VarType RustVarType]
argTypesFromContext args =
    case args of
    [] -> return []
    (x : xs) -> mapM getVarType (x : xs)


getVarType :: SubC.ConvertM m => Sub.Expr -> m (VarType RustVarType)
getVarType (Sub.Var bnd (Just ty)) = error $ "We try to get the type for variable that already is typed." <>
                                          "This is an error, because we should only try to type varaible usage which cannot be annotated in Rust"
getVarType (Sub.Var bnd Nothing) = do
  ctxt <- get
  let argtype = HM.lookup bnd ctxt
  case argtype of
    Just (Sub.RustType rustType) -> return $ asHostNormal rustType
    Nothing -> return $ asHostUnknown-- error $ "Error: No type info found for " <> show bnd
-- FIXME: Actually literal should just carry through the value and the type of the literal
getVarType (Sub.Lit lit) = case lit of
  Sub.Bool b -> return $ asHostNormal rustBool
  Sub.Int i ->  return $ asHostNormal rustI32
  -- Sub.String s ->  return $ asHostNormal PathTy Nothing (Path False [PathSegment "String" Nothing ()] ()) ()
  -- ToDo: Add other literals
getVarType e = error $ "Error: No type info found for " <> show e

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
        -- ToDo: When we thread return type annotations to type functions OR use the extracted function type we need ot distinguish two cases
        --       1. we had no annotation 
        --          -> we set the currentReturn in the context to 'TypeVar' 
        --          -> we try to retrieve arg types (which we will have from the context anyways as we're going top down) and the return type for the function
        --          -> in here we do nothing i.e. just produce a function with return type 'TypeVar', because we want to give a useful error message
        --          however, when we're back at the statement context and the currentType in the context is still 'TypeVar', we error and indicate that the currently 
        --          assigned variable needs to be annotated
        ty <- argTypesFromContext args
        retTy <- return $ asHostUnknown -- get this from context later
        return $ LitE (FunRefLit (FunRef ref Nothing $ FunType ty retTy))
      -- Currently we don't extract function types correctyl so we still need to type them the good old way
      VarE bnd _ty -> do
        let qBnd = toQualBinding bnd
        -- traceM $ "Parsing function " <> show bnd <> " In Context : \n" <> show ctxt <> "\n"
        ty <- argTypesFromContext args
        retTy <- return $ asHostUnknown
        return $ LitE (FunRefLit (FunRef qBnd Nothing $ FunType ty retTy))
      _ -> return fun'
    args' <- mapM convertExpr args
    return $ fun'' `AppE` args'
  -- ToDo: Type function literal here 
  convertExpr (Sub.MethodCall receiver (Sub.CallRef method _) args) = do
    ctxt <- get
    receiver' <- convertExpr receiver
    argTys <- argTypesFromContext args
    receiverTy <- getVarType receiver
    retTy <- return $ asHostUnknown
    -- traceM $ "Context at parsing function " <> show method <> ": \n" <> show ctxt <> "\n"
    let method' = LitE (FunRefLit (FunRef method Nothing $ STFunType receiverTy argTys retTy))
    args' <- mapM convertExpr args
    return $ BindE receiver' method' `AppE` args'
  convertExpr (Sub.Tuple vars) = do
    vars' <- mapM convertExpr vars
    argTys <- argTypesFromContext vars
    -- A tuple constructor is a function that takes agrs and return a tuple of them
    let funTy = case argTys of
            [] -> FunType [] TypeUnit
            (t: ts) -> FunType argTys (TupleTy (t:| ts))
    return $ TupE funTy vars'
  convertExpr (Sub.Binary op left right) = do
    left' <- convertExpr left
    right' <- convertExpr right
    types <- argTypesFromContext [left, right]
    let (symbol, retTy) = binOpInfo op
    funref <- asFunRef symbol types retTy
    return $  AppE funref [left', right']
  convertExpr (Sub.Unary op arg) = do
    arg' <- convertExpr arg
    argTy <- argTypesFromContext [arg]
    let (symbol, retTy) = unOpInfo op
    funref <- asFunRef symbol argTy retTy
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
    let endless_loop:() = 
        (\cond -> let result = *loopbody and 'calculate' condition twice* 
                               in if condition1 { endless_loop condition2 } else {result}  
        ) 
        in  endless_loop true
    -}
    -- FIXME: the type of the loop is not () but bool -> () AND it is not a simple variable but a function
    -- Reminder: The result and input type are () only because we do not thread the states explicitly
    let loopRef = "endless_loop"
    let argUnit =  asHostNormal rustUnitReturn
    let argBool =  asHostNormal rustBool
    let condRef = "cond"
    let condFunType = FunType [TypeBool] (TupleTy (TypeBool:| [TypeBool])) -- we use this to duplicate the condition hence it returns two bools
    let condFun = LitE (FunRefLit (FunRef (toQualBinding "host_id") Nothing condFunType))
    let resultRef = "result"
    let condResultRef = "localCondition"
    let condResultReuse = "copyLocalCond"
    return $
        LetE
        (VarP loopRef argUnit)
        (LamE [VarP condRef argBool] $
          LetE (VarP resultRef argUnit) body' $
          LetE (TupP (VarP condResultRef argBool :| [VarP condResultReuse argBool])) (AppE condFun [VarE condRef argBool])
            (IfE
              (VarE condResultRef argBool)
              (AppE (VarE loopRef argUnit)  [VarE condResultReuse argBool])
              (VarE resultRef argUnit)
        ))
      (AppE (VarE loopRef argUnit) [LitE $ BoolLit True])

  convertExpr (Sub.Closure _ _ _ args _retTy body) = do
    -- currently, we do not have support to pass the return type of a closure around
    ctxt <- get
    args' <- mapM convertPat args
    body' <- convertExpr body
    put ctxt -- make sure that the variables are removed again
    return $ LamE args' body'
  convertExpr (Sub.BlockExpr block) = convertExpr block
  convertExpr (Sub.PathExpr (Sub.CallRef ref tyInfo)) = do
    -- TODO handle type info (these are bound type parameters)
    -- REMINDER: This is (one) origin of Function References. Check what kind of type annotations may already be 
    --           present here and incoorporate them
    -- FIXME: We need the context here, i.e. either we have the function name in the pre-extracted function context, or we 
    --        need to convert Path's only in the context they apprear in, i.e. we need to know with which arguments they are called and 
    --        we need to know the current return type 
    -- Check where CallRefs are produced and how we can constrain there occurence.
    ctxt <- get
    return $ LitE $ FunRefLit $ FunRef ref Nothing $ FunType [asHostUnknown] (asHostUnknown)
  convertExpr (Sub.Var bnd (Just (Sub.RustType ty))) = return $ VarE bnd (asHostNormal ty)
  convertExpr (Sub.Var bnd Nothing) = do
    -- traceM $ "Parsing var " <> show bnd
    ctxt <- get
    case HM.lookup bnd ctxt of
      Just (Sub.RustType rustType) -> return $ VarE bnd  (asHostNormal rustType)
      -- We also get here when we convert function names, for which we currently have no proper typing in place so
      -- for now I'll insert a placeholder type, that is replaces as we return from here to typing the Call expression
      -- ToDo: Fix type when we have proper function type extraction again
      Nothing -> {-(trace $ "Trying to type Variable " <> show bnd )-} return $ VarE bnd (asHostUnknown) -- error $ "No type info found for " <> show bnd

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
      convertStmt :: SubC.ConvertM m => Sub.Stmt -> m (FrLang.Expr RustVarType -> FrLang.Expr RustVarType)
      convertStmt s@(Sub.Local pat ty e) = do
        case (pat, ty) of
          -- ToDo: We should move this to Rust -> Sub Conversion and set it automatically if the RHS is a literal
          (Sub.IdentP (Sub.IdentPat _mode bnd), Just ty') -> modify (HM.insert bnd ty')
          (Sub.TupP idents, Just (Sub.RustType (TupTy tys _))) -> do
            let asSubRustType = map Sub.RustType tys
                asBindings = map fromIdent idents
                keysAndValues = zip asBindings asSubRustType
            modify (\context-> foldl (\c (a,b) ->  HM.insert a b c) context keysAndValues)
          untypedpattern -> return () --ToDo: Should we warn here? logWarnN $ "Targets of the assignment: " <> show s <> " are not annotated\n"
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
fromIdent (Sub.IdentPat _mode bnd) =  bnd

instance ConvertPat Sub.Pat where
  convertPat Sub.WildP   = return $ VarP (fromString "_") (asHostNormal rustInfer)
  convertPat (Sub.IdentP ip) = convertPat ip
  -- It is possible to bind: let () = e1; in Rust, iff e1 evaluates to ()
  -- The pattern in this case would be TupP [], because language-rust has no Unit pattern
  -- However we do so we distinguish cases here 
  convertPat (Sub.TupP []) = return $ FrLang.WildP TypeUnit
  convertPat (Sub.TupP (pt: pts)) = do
    pt' <- convertPat pt
    pts' <- mapM convertPat pts
    return $ TupP (pt':| pts')

instance ConvertPat Sub.IdentPat where
  -- We will mostly get here when a pattern is a local bound variable, in which case it is
  -- in the context already and we can retrieve it's type
  -- However this pattern may also be bound in a for loop, where we could only infer the type
  -- from later usage. For now I'll handle the second case with infered type, because as soon
  -- as the pattern should be used somewhere i.e. when we need the type, an annotated assignment of 
  -- the pattern to a local variable is required or the compilation fails. Hence '_' type shouldn't hurt for now
  -- FIXME: Can we get complete Typing for patterns?
  convertPat (Sub.IdentPat mutability bnd) = do
    ctxt <- get
    let ty = HM.lookup bnd ctxt
    case ty of
      Just (Sub.RustType rustType) -> return $ VarP bnd (asHostNormal rustType)
      Nothing -> (trace $ "No type info found for pattern " <> show bnd) return $ VarP bnd (asHostUnknown)

instance ConvertPat Sub.Arg where
  convertPat (Sub.Arg pat ty) = do
    case pat of
      -- Reminder: Actually we should either remove one of the type sources
      --           or at least ensure they are equal
      Sub.IdentP (Sub.IdentPat _ bnd) -> modify (HM.insert bnd ty)
      _ -> return ()
    convertPat pat

binOpInfo:: Sub.BinOp -> (Binding, VarType RustVarType)
-- ToDo: It seems infering the type of binary ops should be easy given the input. However
--       we might need to incorparate Rusts coercion rules if the two arguments differ in type
  -- Check if that applicable and if it would be a problem after all since when somethigncan be coerced for this binary op, it can also be coerced downstream probably, if we just take one of the types.
binOpInfo Sub.Add =  ("+", asHostUnknown)
binOpInfo Sub.Sub =  ("-", asHostUnknown)
binOpInfo Sub.Mul =  ("*", asHostUnknown)
binOpInfo Sub.Div =  ("/", asHostUnknown)
binOpInfo Sub.Lt =  ("<", asHostUnknown)
binOpInfo Sub.Lte =  ("<=", asHostNormal rustBool)
binOpInfo Sub.Gte =  (">=", asHostNormal rustBool)
binOpInfo Sub.Gt =  (">", asHostNormal rustBool)
binOpInfo Sub.EqOp =  ("==", asHostNormal rustBool)
binOpInfo Sub.OrOp =  ("||", asHostNormal rustBool)

unOpInfo::Sub.UnOp -> (Binding, VarType RustVarType)
unOpInfo Sub.Deref =  ("*",  asHostUnknown)
-- ToDo: negation is a trait and returns whatever the input type implemented it to return i.e. not necessarily of the same type
unOpInfo Sub.Not =  ("!",  asHostUnknown)
unOpInfo Sub.Neg =  ("-",  asHostUnknown)

asFunRef :: Monad m => Binding -> [VarType RustVarType] -> VarType RustVarType -> m (FrLang.Expr RustVarType)
asFunRef op tys retTy = return $
      LitE $ FunRefLit $
        FunRef (QualifiedBinding (makeThrow []) op) Nothing (FunType tys retTy)


toQualBinding :: Binding -> QualifiedBinding
toQualBinding = QualifiedBinding (makeThrow [])

instance ConvertExpr Sub.Lit where
  convertExpr (Sub.Int i) = return $ LitE $ NumericLit i
  convertExpr (Sub.Bool b) = return $ LitE $ BoolLit b


realPatType:: FrLang.Pat RustVarType -> Maybe (VarType RustVarType)
realPatType = \case
      FrLang.VarP _bnd  ty -> if isUnknown ty then Nothing else Just ty
      FrLang.WildP ty -> if isUnknown ty then Nothing else Just ty
      tp@(FrLang.TupP pats) ->
            let TupleTy tys = FrLang.patType tp
            in if any TE.isUnknown tys then Nothing else Just (TupleTy tys)

