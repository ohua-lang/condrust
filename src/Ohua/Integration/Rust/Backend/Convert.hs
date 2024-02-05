module Ohua.Integration.Rust.Backend.Convert (
  module Ohua.Integration.Rust.Backend.Convert,
  module Ohua.Integration.Rust.Common.Convert
  )where

import Data.List.NonEmpty as NE hiding (map)
import Data.Text (unpack)
import Language.Rust.Data.Ident
import Language.Rust.Syntax as Rust hiding (Rust)
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import Ohua.Integration.Rust.Common.Convert
import Ohua.Prelude
import Ohua.Types.Vector (natToInt)
import GHC.Base (undefined)

convertExp :: Sub.Expr -> Rust.Expr ()
convertExp (Sub.Var bnd) =
  PathExpr
    []
    Nothing
    (Path False [PathSegment (mkIdent $ unpack $ unwrap bnd) Nothing noSpan] noSpan)
    noSpan
convertExp (Sub.Lit (NumericLit i)) = Rust.Lit [] (Int Dec i Unsuffixed noSpan) noSpan
convertExp (Sub.Lit (BoolLit b)) = Rust.Lit [] (Bool b Unsuffixed noSpan) noSpan
convertExp (Sub.Lit (StringLit str)) = Rust.Lit [] (Str str Cooked Unsuffixed noSpan) noSpan
convertExp (Sub.Lit UnitLit) =
  TupExpr [] [] noSpan
convertExp (Sub.Lit (HostLit _hostExpr _ty)) =
  error "ToDo: We need to implement support for Host expressions in the Rust Backend. Please remind us :-)"
-- Question: Why is this an error?
convertExp (Sub.Lit (EnvRefLit _hostExpr _ty)) =
  error "Host expression encountered! This is a compiler error. Please report!"
convertExp (Sub.Lit (FunRefLit (FunRef qBnd _type))) =
  PathExpr [] Nothing (convertQualBnd qBnd) noSpan
convertExp (Sub.Call cr args) =
  Call
    []
    (convertCallRef cr)
    (map convertExp args)
    noSpan
convertExp (Sub.MethodCall stateExpr (Sub.CallRef (QualifiedBinding _ bnd) ty) args) =
  MethodCall
    []
    (convertExp stateExpr)
    (PathSegment (mkIdent $ unpack $ unwrap bnd) (convertGenericArgs <$> ty) noSpan)
    (map convertExp args)
    noSpan
convertExp (Sub.Binary op e1 e2) =
  Rust.Binary [] (convertBinary op) (convertExp e1) (convertExp e2) noSpan
convertExp (Sub.Unary op e) =
  Rust.Unary [] (convertUnary op) (convertExp e) noSpan
convertExp (Sub.Assign e1 e2) =
  Rust.Assign [] (convertExp e1) (convertExp e2) noSpan
convertExp (Sub.Loop block) =
  Rust.Loop [] (convertBlock block) Nothing noSpan
convertExp (Sub.ForLoop p r block) =
  let p' = convertIdentPat p
      r' = convertExp r
      block' = convertBlock block
   in Rust.ForLoop [] p' r' block' Nothing noSpan
convertExp (Sub.While e b) =
  Rust.While [] (convertExp e) (convertBlock b) Nothing noSpan
convertExp (Sub.If condExpr trueBranch falseBranch) =
  Rust.If
    []
    (convertExp condExpr)
    (convertBlock trueBranch)
    (convertExp <$> falseBranch)
    noSpan
convertExp (Sub.Tuple es) =
  Rust.TupExpr [] (map convertExp es) noSpan
convertExp (Sub.TupleField e n) =
  Rust.TupField [] (convertExp e) (natToInt n) noSpan
convertExp (Sub.Try e) = Rust.Try [] (convertExp e) noSpan
convertExp (Sub.BlockExpr block) =
  Rust.BlockExpr [] (convertBlock block) Nothing noSpan
convertExp (Sub.HalfOpenRange lower upper) =
  Rust.Range [] (convertExp <$> lower) (convertExp <$> upper) HalfOpen noSpan
convertExp (Sub.Async block) =
  Rust.Async [] Rust.Value (convertBlock block) noSpan

prependToBlock :: [Sub.Stmt] -> Sub.Expr -> Sub.Expr
prependToBlock stmts cont =
  case cont of
    Sub.BlockExpr (Sub.RustBlock unsafety contStmts) ->
      Sub.BlockExpr $ Sub.RustBlock unsafety (stmts ++ contStmts)
    _ -> Sub.BlockExpr $ Sub.RustBlock Sub.Normal (stmts ++ [Sub.NoSemi cont])

convertBlock :: Sub.Block -> Rust.Block ()
convertBlock (Sub.RustBlock unsafety stmts) =
  Rust.Block (map convertStmt stmts) (convertUnsafety unsafety) noSpan

convertUnsafety :: Sub.Unsafety -> Rust.Unsafety
convertUnsafety Sub.Normal = Rust.Normal

convertStmt :: Sub.Stmt -> Rust.Stmt ()
convertStmt (Sub.Semi e) = Rust.Semi (convertExp e) noSpan
convertStmt (Sub.NoSemi e) = Rust.NoSemi (convertExp e) noSpan
convertStmt (Sub.Local p ty e) =
  Local (convertPat p) Nothing (Just $ convertExp e) [] noSpan
convertStmt Sub.StandaloneSemi = Rust.StandaloneSemi noSpan

convertPat :: Sub.Pat -> Rust.Pat ()
convertPat (Sub.IdentP p) = convertIdentPat p
convertPat (Sub.TupP ps) = Rust.TupleP (map convertIdentPat ps) noSpan
convertPat Sub.WildP = Rust.WildP noSpan

convertIdentPat :: Sub.IdentPat -> Rust.Pat ()
convertIdentPat (Sub.IdentPat b bnd) =
  Rust.IdentP (convertBindingMode b) (mkIdent $ unpack $ unwrap bnd) Nothing noSpan

convertBindingMode :: Sub.BindingMode -> Rust.BindingMode
convertBindingMode Sub.Mutable = Rust.ByValue Rust.Mutable
convertBindingMode Sub.Immutable = Rust.ByValue Rust.Immutable

convertQualBnd :: QualifiedBinding -> Path ()
convertQualBnd (QualifiedBinding ns bnd) =
  Path
    False
    ( map
        (\p ->
            if p == "." 
              then   PathSegment (mkIdent "crate") Nothing noSpan
              else   PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
            
        $ unwrap ns ++ [bnd]
    )
    noSpan

convertRef :: QualifiedBinding -> Maybe Sub.GenericArgs -> Path ()
convertRef f ty =
  let (Path b segs s) = convertQualBnd f
      attach (PathSegment i _ s) = PathSegment i (convertGenericArgs <$> ty) s
      segs' = case segs of
        [] -> []
        (a : as) ->
          let (l :| prev) = NE.reverse $ a :| as
           in NE.toList $ NE.reverse $ attach l :| prev
   in Path b segs' s

convertCallRef :: Sub.CallRef -> Expr ()
convertCallRef (Sub.CallRef f ty) =
   PathExpr [] Nothing (convertRef f ty) noSpan

convertTyRef :: Sub.TyRef -> Ty ()
convertTyRef (Sub.TyRef f ty) =
   PathTy Nothing (convertRef f ty) noSpan

convertGenericArgs :: Sub.GenericArgs -> GenericArgs ()
convertGenericArgs (Sub.AngleBracketed args) = 
  AngleBracketed (map convertGenericArg args) [] noSpan
-- The optional type reference is an output type
convertGenericArgs (Sub.Parenthesized tyRefs (Just tRef)) = Parenthesized (map convertToTy tyRefs) (Just $ convertToTy tRef) noSpan
convertGenericArgs (Sub.Parenthesized tyRefs Nothing ) = Parenthesized (map convertToTy tyRefs) Nothing noSpan

convertGenericArg :: Sub.GenericArg -> GenericArg ()
convertGenericArg (Sub.TypeArg (Sub.RustType ty)) = TypeArg ty

convertBinary :: Sub.BinOp -> Rust.BinOp
convertBinary Sub.Add = Rust.AddOp
convertBinary Sub.Sub = Rust.SubOp
convertBinary Sub.Mul = Rust.MulOp
convertBinary Sub.Div = Rust.DivOp
convertBinary Sub.Lt  = Rust.LtOp
convertBinary Sub.Lte = Rust.LeOp
convertBinary Sub.Gt  = Rust.GtOp
convertBinary Sub.Gte = Rust.GeOp
convertBinary Sub.EqOp = Rust.EqOp
convertBinary Sub.OrOp = Rust.OrOp

convertUnary :: Sub.UnOp -> Rust.UnOp
convertUnary Sub.Not = Rust.Not
convertUnary Sub.Neg = Rust.Neg
convertUnary Sub.Deref = Rust.Deref

convertToTy :: Sub.TyRef -> Ty ()
--FIXME: Must not be unfdefined
convertToTy (Sub.TyRef qb Nothing) = error "Conversion of generic parameters is not fully implemented. Please file a bug"
convertToTy (Sub.TyRef qb (Just gPs)) = error "Conversion of generic parameters is not fully implemented. Please file a bug"