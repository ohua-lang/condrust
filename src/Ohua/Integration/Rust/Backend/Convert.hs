module Ohua.Integration.Rust.Backend.Convert where

import Data.List.NonEmpty as NE hiding (map)
import Data.Text (unpack)
import Language.Rust.Data.Ident
import Language.Rust.Syntax as Rust hiding (Rust)
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import Ohua.Prelude
import Ohua.Types.Vector (natToInt)

convertBlock :: Sub.Block -> Rust.Block ()
convertBlock (Sub.Block stmts) = Rust.Block (map convertStmt stmts) Normal noSpan

convertExpr :: Sub.Expr -> Rust.Expr ()
convertExpr (Sub.Var bnd) =
  PathExpr
    []
    Nothing
    (Path False [PathSegment (mkIdent $ unpack $ unwrap bnd) Nothing noSpan] noSpan)
    noSpan
convertExpr (Sub.Lit (NumericLit i)) = Rust.Lit [] (Int Dec i Unsuffixed noSpan) noSpan
convertExpr (Sub.Lit (BoolLit b)) = Rust.Lit [] (Bool b Unsuffixed noSpan) noSpan
convertExpr (Sub.Lit UnitLit) =
  TupExpr [] [] noSpan -- FIXME this means *our* unit, I believe.
convertExpr (Sub.Lit (EnvRefLit _hostExpr)) =
  error "Host expression encountered! This is a compiler error. Please report!"
convertExpr (Sub.Lit (FunRefLit (FunRef qBnd _ _type))) =
  PathExpr [] Nothing (convertQualBnd qBnd) noSpan
convertExpr (Sub.Call cr args) =
  Call
    []
    (convertCallRef cr)
    (map convertExpr args)
    noSpan
convertExpr (Sub.MethodCall stateExpr (Sub.CallRef (QualifiedBinding _ bnd) ty) args) =
  MethodCall
    []
    (convertExpr stateExpr)
    (PathSegment (mkIdent $ unpack $ unwrap bnd) ty noSpan)
    (map convertExpr args)
    noSpan
convertExpr (Sub.Binary op e1 e2) =
  Rust.Binary [] (convertBinary op) (convertExpr e1) (convertExpr e2) noSpan
convertExpr (Sub.Unary op e) =
  Rust.Unary [] (convertUnary op) (convertExpr e) noSpan
convertExpr (Sub.Assign e1 e2) =
  Rust.Assign [] (convertExpr e1) (convertExpr e2) noSpan
convertExpr (Sub.Loop block) =
  Rust.Loop [] (convertBlock block) Nothing noSpan
convertExpr (Sub.ForLoop p r block) =
  let p' = convertIdentPat p
      r' = convertExpr r
      block' = convertBlock block
   in Rust.ForLoop [] p' r' block' Nothing noSpan
convertExpr (Sub.While e b) =
  Rust.While [] (convertExpr e) (convertBlock b) Nothing noSpan
convertExpr (Sub.If condExpr trueBranch falseBranch) =
  Rust.If
    []
    (convertExpr condExpr)
    (convertBlock trueBranch)
    (convertExpr <$> falseBranch)
    noSpan
convertExpr (Sub.Tuple es) =
  Rust.TupExpr [] (map convertExpr es) noSpan
convertExpr (Sub.TupleField e n) =
  Rust.TupField [] (convertExpr e) (natToInt n) noSpan
convertExpr (Sub.Try e) = Rust.Try [] (convertExpr e) noSpan
convertExpr (Sub.BlockExpr block) =
  Rust.BlockExpr [] (convertBlock block) Nothing noSpan
convertExpr (Sub.HalfOpenRange lower upper) =
  Rust.Range [] (convertExpr <$> lower) (convertExpr <$> upper) HalfOpen noSpan
convertExpr (Sub.Async block) =
  Rust.Async [] Rust.Value (convertBlock block) noSpan

convertStmt :: Sub.Stmt -> Rust.Stmt ()
convertStmt (Sub.Semi e) = Rust.Semi (convertExpr e) noSpan
convertStmt (Sub.NoSemi e) = Rust.NoSemi (convertExpr e) noSpan
convertStmt (Sub.Local p e) =
  Local (convertPat p) Nothing (Just $ convertExpr e) [] noSpan

convertPat :: Sub.Pat -> Rust.Pat ()
convertPat (Sub.IdentP p) = convertIdentPat p
convertPat (Sub.TupP ps) = Rust.TupleP (map convertIdentPat ps) noSpan

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
        (\p -> PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
        $ unwrap ns ++ [bnd]
    )
    noSpan

convertCallRef :: Sub.CallRef -> Expr ()
convertCallRef (Sub.CallRef f ty) =
  let (Path b segs s) = convertQualBnd f
      attach (PathSegment i _ s) = PathSegment i ty s
      segs' = case segs of
        [] -> []
        (a : as) ->
          let (l :| prev) = NE.reverse $ a :| as
           in NE.toList $ NE.reverse $ attach l :| prev
   in PathExpr [] Nothing (Path b segs' s) noSpan

convertBinary :: Sub.BinOp -> Rust.BinOp
convertBinary Sub.Add = Rust.AddOp
convertBinary Sub.Sub = Rust.SubOp
convertBinary Sub.Mul = Rust.MulOp
convertBinary Sub.Div = Rust.DivOp

convertUnary :: Sub.UnOp -> Rust.UnOp
convertUnary Sub.Not = Rust.Not
convertUnary Sub.Neg = Rust.Neg
convertUnary Sub.Deref = Rust.Deref

noSpan :: ()
noSpan = ()

prependToBlock :: [Sub.Stmt] -> Sub.Expr -> Sub.Expr
prependToBlock stmts cont =
  case cont of
    Sub.BlockExpr (Sub.Block contStmts) ->
      Sub.BlockExpr $ Sub.Block $ stmts ++ contStmts
    _ -> Sub.BlockExpr $ Sub.Block $ stmts ++ [Sub.NoSemi cont]
