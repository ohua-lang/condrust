{-# LANGUAGE ConstraintKinds #-}
module Ohua.Integration.Rust.Frontend.Convert where

import Language.Rust.Data.Ident
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Integration.Rust.Common.Convert
import qualified Ohua.Integration.Rust.Frontend.Subset as Sub
import Ohua.Prelude

import qualified Data.HashMap.Lazy as HM

type Context = HM.HashMap Binding Sub.RustType

type ConvertM m = (Monad m, MonadState Context m)

convertExpr :: ConvertM m => Rust.Expr Span -> m Sub.Expr
convertExpr e@Box {} = error $ "Currently, we do not support the construction of boxed values. Please do so in a function." <> show e
convertExpr e@Vec {} = error $ "Currently, we do not support array expressions. Please do so in a function.\n" <> show e
convertExpr (Call [] fun args _) = do
  -- FIXME_L: If we allready fill the context in this conversion, we should have the
  --          argument types as well as the return type of a call available, when we parse the call because 
  --          we convert algos top down
  fun' <- convertExpr fun
  args' <- mapM convertExpr args
  return $ Sub.Call fun' args'
convertExpr e@Call {} = error $ "Currently, we do not support attributes on function calls.\n" <> show e
convertExpr (MethodCall [] receiver method args _) = do
  receiver' <- convertExpr receiver
  method' <- convertLastSegment method
  let method'' = case method' of
        (n, ty) ->
          Sub.CallRef
            (QualifiedBinding (makeThrow []) $ fromString n)
            ty
  args' <- mapM convertExpr args
  return $ Sub.MethodCall receiver' method'' args'
convertExpr e@MethodCall {} = error $ "Currently, we do not support attributes on method calls.\n" <> show e
convertExpr (TupExpr [] vars _) = do
  vars' <- mapM convertExpr vars
  return $ Sub.Tuple vars'
convertExpr e@TupExpr {} = error $ "Currently, we do not support attributes on tuple expressions.\n" <> show e
convertExpr (Binary [] op left right _) = do
  op' <- convertBinOp op
  left' <- convertExpr left
  right' <- convertExpr right
  return $ Sub.Binary op' left' right'
convertExpr e@Binary {} = error $ "Currently, we do not support attributes on binary operations.\n" <> show e
convertExpr (Unary [] op arg _) = do
  let op' = convertUnOp op
  arg' <- convertExpr arg
  return $ Sub.Unary op' arg'
convertExpr e@Unary {} = error $ "Currently, we do not support attributes on unary operations.\n" <> show e
convertExpr (Lit [] l _) = Sub.Lit <$> convertLit l
convertExpr e@Lit {} = error $ "Currently, we do not support attributes on literals operations.\n" <> show e
convertExpr e@Cast {} = error $ "Currently, we do not support cast expressions. Please use a function.\n" <> show e
convertExpr e@TypeAscription {} = error $ "Currently, we do not support type ascriptions. Please use a function.\n" <> show e
convertExpr (If [] expr trueBlock falseBlock _) = do
  expr' <- convertExpr expr
  trueBlock' <- convertBlock trueBlock
  falseBlock' <- mapM convertExpr falseBlock
  return $ Sub.If expr' trueBlock' falseBlock'
convertExpr e@If {} = error $ "Currently, we do not support attributes on conditional expressions.\n" <> show e
convertExpr e@IfLet {} = error $ "Currently, we do not support if-let expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
convertExpr (While [] cond block Nothing _) = do
  cond' <- convertExpr cond
  block' <- convertBlock block
  return $ Sub.While cond' block'
convertExpr e@(While [] _ _ (Just _) _) = error $ "Currently, we do not support loop labels.\n" <> show e
convertExpr e@While {} = error $ "Currently, we do not support attributes on while loops.\n" <> show e
convertExpr e@WhileLet {} = error $ "Currently, we do not support if-let expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
convertExpr (ForLoop [] pat dataExpr body Nothing _) = do
  pat' <- convertPat pat
  dataExpr' <- convertExpr dataExpr
  body' <- convertBlock body
  return $ Sub.ForLoop pat' dataExpr' body'
convertExpr e@(ForLoop [] _ _ _ (Just _) _) = error $ "Currently, we do not support loop labels.\n" <> show e
convertExpr e@ForLoop {} = error $ "Currently, we do not support attributes on for loops.\n" <> show e
convertExpr e@(Loop _ body _ _) = do
  body' <- convertBlock body
  return $ Sub.EndlessLoop body'
convertExpr e@Match {} = error $ "Currently, we do not support match expressions. Please file a bug if you feel that this is dearly needed.\n" <> show e
convertExpr (Closure [] Value NotAsync Movable (FnDecl args retTy False _) body _) = do
  args' <- mapM convertArg args
  let retTy' = Sub.RustType . (noSpan <$) <$> retTy
  body' <- convertExpr body
  return $ Sub.Closure Sub.Value Sub.NotAsync Sub.Movable args' retTy' body'
convertExpr e@(Closure _ _ _ _ (FnDecl _ _ True _) _ _) = error $ "Currently, we do not support variadic argument lists. \n" <> show e
convertExpr e@(Closure _ _ _ Immovable _ _ _) = error $ "Currently, we do not support immovable closures. \n" <> show e
convertExpr e@(Closure _ Ref _ _ _ _ _) = error $ "Currently, we do not support closures that capture environment variables by reference. \n" <> show e
convertExpr e@(Closure _ _ IsAsync _ _ _ _) = error $ "Async functions are not part of the supported Rust subset. \n" <> show e
convertExpr e@Closure {} = error $ "Currently, we do not support attributes on closures.\n" <> show e
convertExpr e@(BlockExpr [] block Nothing _) = Sub.BlockExpr <$> convertBlock block
convertExpr e@(BlockExpr _ _ (Just _) _) = error $ "Labels are not part of the supported Rust subset.\n" <> show e
convertExpr e@BlockExpr {} = error $ "Currently, we do not support attributes on block expressions.\n" <> show e
convertExpr e@TryBlock {} = error $ "Currently, we do not support try-block expressions. Please use a function. \n" <> show e
convertExpr e@Async {} = error $ "Async is not part of the supported Rust subset. \n" <> show e
convertExpr e@Await {} = error $ "Async is not part of the supported Rust subset. \n" <> show e
convertExpr e@Assign {} = error $ "Currently, we do not support assign expressions (because memory is managed inside the functions). Please use a function. \n" <> show e
convertExpr e@AssignOp {} = error $ "Currently, we do not support assign-op expressions (because memory is managed inside the functions). Please use a function. \n" <> show e
convertExpr e@FieldAccess {} = error $ "Currently, we do not support field access expressions (because memory/state is managed inside the functions). Please use a function. \n" <> show e
convertExpr e@TupField {} = error $ "Currently, we do not support tuple field expressions. Please use a function. \n" <> show e
convertExpr e@Index {} = error $ "Currently, we do not support indexing expressions. Please use a function. \n" <> show e
convertExpr e@Range {} = error $ "Currently, we do not support range expressions. Please use a function. \n" <> show e
convertExpr (PathExpr [] Nothing path _) = do
  path' <- convertPath path
  return $ case path' of
    -- We don't know the type of this variable yet, we need to retrieve it later from the context
    Left v -> Sub.Var v Nothing
    Right cr -> Sub.PathExpr cr
convertExpr e@(PathExpr [] (Just _) _ _) = error $ "Currently, we do not support paths to 'self', i.e., compilation of 'impl' functions. \n" <> show e
convertExpr e@PathExpr {} = error $ "Currently, we do not support attributes on path expressions.\n" <> show e
convertExpr e@AddrOf {} = error $ seqParProgNote <> "\n" <> show e
convertExpr e@Break {} = error $ "Currently, we do not support 'break' expressions. Please reformulate the loop. \n" <> show e
convertExpr e@Continue {} = error $ "Currently, we do not support 'continue' expressions. Please reformulate the loop. \n" <> show e
convertExpr e@Ret {} = error $ "Currently, we do not support 'return' expressions. Please reformulate into an expression without a semicolon. \n" <> show e
convertExpr e@MacExpr {} = error $ "Currently, we do not support macro invocations. \n" <> show e
convertExpr e@Struct {} = error $ "Currently, we do not support struct literal expressions. Please use a function.\n" <> show e
convertExpr e@Repeat {} = error $ "Currently, we do not support array construction expressions. Please use a function.\n" <> show e
convertExpr e@ParenExpr {} = error $ "Impossible per documentation of language-rust.\n" <> show e
convertExpr e@Try {} = error $ "Currently, we do not support error handling expressions. Please use a function.\n" <> show e
convertExpr e@Yield {} = error $ "Currently, we do not support generator/yield expressions. Please use a function.\n" <> show e

convertBlock :: ConvertM m => Rust.Block Span -> m Sub.Block
convertBlock b@(Block _ Unsafe _) = error $ "Currently, we do not support unsafe blocks.\n" <> show b
convertBlock (Block stmts Rust.Normal _) = (Sub.RustBlock Sub.Normal) <$> mapM convertStmt stmts

convertStmt :: ConvertM m => Stmt Span -> m Sub.Stmt
convertStmt (Local pat ty (Just e) [] _) = do
  pat' <- convertPat pat
  let ty' = Sub.RustType . void <$> ty
  e' <- convertExpr e
  return $ Sub.Local pat' ty' e'
convertStmt s@(Local pat _ Nothing _ _) =
  error $ "Variables bind values and as such they need to be initialized. \n" <> show s
convertStmt s@Local {} =
  error $ "Currently, we do not support attributes on local bindings.\n" <> show s
convertStmt s@ItemStmt {} =
  error $ "Currently, we do not support item statements.\n" <> show s
convertStmt (NoSemi e _) = Sub.NoSemi <$> convertExpr e
convertStmt (Semi e _) = Sub.Semi <$> convertExpr e
convertStmt s@MacStmt {} =
  error $ "Currently, we do not support macro calls.\n" <> show s
convertStmt StandaloneSemi{} = return Sub.StandaloneSemi

convertPath ::
  ConvertM m =>
  Rust.Path Span ->
  m (Either Sub.VarRef Sub.CallRef)
convertPath p@(Path _ segments _) =
  case segments of
    [segment] -> do
      refname <- convertPathSegment segment  
      return $ Left $ fromString refname
    (seg : rest) -> do
      let segs = seg :| rest
      let firstSegs = init segs
      let lastSeg = last segs
      firstSegs' <- mapM (fmap fromString . convertPathSegment) firstSegs
      (lastSeg', ty) <- convertLastSegment lastSeg
      return $
        Right $
          Sub.CallRef
            (QualifiedBinding (makeThrow firstSegs') $ fromString lastSeg')
            ty
    [] -> error $ "I received a path with no segments. I do not konw what to do with it: " <> show p

convertPathSegment (PathSegment Ident {name = n} Nothing _) = return n
convertPathSegment e@PathSegment {} = error $ "Currently, we support type parameters only on the last element of the path.\n" <> show e

convertLastSegment (PathSegment Ident {name = n} ty _) = (n,) <$> mapM convertGenericArgs ty

convertGenericArgs :: ConvertM m => GenericArgs Span -> m Sub.GenericArgs
convertGenericArgs (AngleBracketed args [] _) = Sub.AngleBracketed <$> mapM convertGenericArg args
convertGenericArgs a@(AngleBracketed _ as _) = error $ "Currently, we do not support type constraints: " <> show a
convertGenericArgs a = error $ "Currently, we only support angle-bracketed type information: " <> show a

convertGenericArg :: ConvertM m => GenericArg Span -> m Sub.GenericArg
convertGenericArg (TypeArg ty) = return $ Sub.TypeArg $ Sub.RustType (noSpan <$ ty)
convertGenericArg l@LifetimeArg{} = error $ "Currently, we do not support lifetime type arguments: " <> show l
convertGenericArg c@ConstArg{} = error $ "Currently, we do not support const type arguments: " <> show c

convertArg :: ConvertM m => Rust.Arg Span -> m Sub.Arg
convertArg (Arg _ (Just p) ty _) = (`Sub.Arg` Sub.RustType (noSpan <$ ty)) <$> convertPat p
convertArg a@(Arg _ Nothing _ _) = error $ "Currently, we require a name for each argument, not only its type. If this is a type definition in your code, then please file a bug.\n" <> show a
convertArg a = error $ "Currently, we do not support self arguments. \n" <> show a

convertPat :: ConvertM  m => Rust.Pat Span -> m Sub.Pat
convertPat (WildP _) = return Sub.WildP
convertPat (IdentP (ByValue Immutable) Ident {name = n, raw = False} Nothing _) =
  return $ Sub.IdentP $ Sub.IdentPat Sub.Immutable $ fromString n
convertPat (IdentP (ByValue Mutable) Ident {name = n, raw = False} Nothing _) =
  return $ Sub.IdentP $ Sub.IdentPat Sub.Mutable $ fromString n
convertPat p@(IdentP _ Ident {raw = True} _ _) = error $ "Qualified identifiers in a pattern are currently not supported. Pattern: " <> show p
convertPat p@(IdentP _ _ (Just _) _) = error $ "Currently, we do not support nested patterns: " <> show p
convertPat p@(IdentP (ByRef _) _ _ _) = error $ seqParProgNote <> "\n" <> show p
convertPat p@StructP {} = error $ "Currently, we do not support struct patterns: " <> show p <> ". Please use a function."
convertPat p@TupleStructP {} = error $ "Currently, we do not support tuple struct patterns: " <> show p <> ". Please use a function."
convertPat p@PathP {} = error $ "Currently, we do not support path patterns: " <> show p
convertPat pat@(TupleP patterns _) =
  let unwrapIdentPat p =
        case p of
          Sub.IdentP p -> return p
          _ -> error $ "Currently, we do not support nested tuple patterns: " <> show pat
   in Sub.TupP <$> mapM (unwrapIdentPat <=< convertPat) patterns
convertPat p@OrP {} = error $ "Currently, we do not support or patterns: " <> show p <> "."
convertPat p@BoxP {} = error $ "Currently, we do not support box patterns: " <> show p <> ". Please use a function."
convertPat p@RefP {} = error $ seqParProgNote <> "\n" <> show p
convertPat p@LitP {} = error $ "Currently, we do not support literal patterns: " <> show p <> ". Please use a function."
convertPat p@RangeP {} = error $ "Currently, we do not support range patterns: " <> show p <> ". Please use a function."
convertPat p@SliceP {} = error $ "Currently, we do not support slice patterns: " <> show p <> ". Please use a function."
convertPat p@RestP {} = error $ "Currently, we do not support rest patterns: " <> show p <> "."
convertPat p@ParenP {} = error $ "Currently, we do not support paren patterns: " <> show p <> "."
convertPat p@MacP {} = error $ "Currently, we do not support patterns resulting from macro expansion: " <> show p <> ". Please use a function."


convertBinOp :: ConvertM m => Rust.BinOp -> m Sub.BinOp
convertBinOp AddOp = return Sub.Add
convertBinOp SubOp = return Sub.Sub
convertBinOp MulOp = return Sub.Mul
convertBinOp DivOp = return Sub.Div
convertBinOp EqOp = return Sub.EqOp
convertBinOp LtOp = return Sub.Lt
convertBinOp LeOp = return Sub.Lte
convertBinOp GeOp = return Sub.Gte
convertBinOp GtOp = return Sub.Gt
convertBinOp OrOp = return Sub.OrOp
convertBinOp o = error $ "Unsupported binary operation: " <> show o

convertUnOp :: Rust.UnOp -> Sub.UnOp
convertUnOp Deref = Sub.Deref
convertUnOp Not = Sub.Not
convertUnOp Neg = Sub.Neg

convertLit :: ConvertM m => Rust.Lit Span -> m Sub.Lit
convertLit (Int Dec i _ _) = return $ Sub.Int i
convertLit (Bool b _ _) = return $ Sub.Bool b
-- ToDo: This seems to be called also when we e.g. return an i32. So even with int and bool -> check why
convertLit _ = error "Currently, we miss proper support for literals. This is a TODO. Please file a bug."



seqParProgNote = "In a sequential program, memory management can be performed at compile-time via the borrowing concept. For a parallel program, this is not easily possible anymore. You will have to move your memory management from compile-time to runtime, i.e., from references to std::sync::Arc. Currently, we do not perform this conversion."
