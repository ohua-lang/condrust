module Ohua.Integration.Rust.Backend.Convert where

import Ohua.Prelude
import Ohua.Types.Vector (natToInt)
import Language.Rust.Data.Ident
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import Language.Rust.Syntax as Rust hiding (Rust)
import Data.Text (unpack)


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
convertExpr (Sub.Call f args) =
  Call
  []
  (convertExpr $ Sub.Lit $ FunRefLit $ FunRef f Nothing Untyped)
  (map convertExpr args)
  noSpan
convertExpr (Sub.MethodCall stateExpr bnd args) =
  MethodCall
  []
  (convertExpr stateExpr)
  (PathSegment (mkIdent $ unpack $ unwrap bnd) Nothing noSpan)
  (map convertExpr args)
  noSpan
convertExpr (Sub.Assign e1 e2) =
  Rust.Assign [] (convertExpr e1) (convertExpr e2) noSpan
convertExpr (Sub.Loop block) =
  Rust.Loop [] (convertBlock block) Nothing noSpan
convertExpr (Sub.ForLoop p r block) =
  let p' = convertIdentPat p
      r' = convertRange r
      block' = convertBlock block
  in Rust.ForLoop [] p' r' block' Nothing noSpan
convertExpr (Sub.While e b) =
  Rust.While [] (convertExpr e) (convertBlock b) Nothing noSpan
convertExpr (Sub.If condExpr trueBranch falseBranch) =
  Rust.If []
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
  (map
    (\p -> PathSegment (mkIdent $ unpack $ unwrap p) Nothing noSpan)
    $ unwrap ns ++ [bnd])
  noSpan

convertRange :: Sub.Range -> Rust.Expr ()
convertRange (Sub.Range lower upper) =
  Rust.Range [] (convertExpr <$> lower) (convertExpr <$> upper) HalfOpen noSpan

noSpan :: ()
noSpan = ()

