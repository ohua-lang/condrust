module Ohua.Integration.Rust.Common.Convert where

import Data.List.NonEmpty as NE hiding (map)
import Data.Text (unpack)
import Language.Rust.Data.Ident
import Language.Rust.Syntax as Rust hiding (Rust)
import qualified Ohua.Integration.Rust.Common.Subset as Sub
import Ohua.Prelude
import Ohua.Types.Vector (natToInt)

class ConvertExpr e where
  convertExpr :: e -> Rust.Expr ()

convertBlock :: ConvertExpr e => Sub.Block e -> Rust.Block ()
convertBlock (Sub.RustBlock stmts unsafety) =
  Rust.Block (map convertStmt stmts) (convertUnsafety unsafety) noSpan

convertUnsafety :: Sub.Unsafety -> Rust.Unsafety
convertUnsafety Sub.Normal = Rust.Normal

convertStmt :: ConvertExpr e => Sub.Stmt e -> Rust.Stmt ()
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
      attach (PathSegment i _ s) = PathSegment i (convertGenericArgs <$> ty) s
      segs' = case segs of
        [] -> []
        (a : as) ->
          let (l :| prev) = NE.reverse $ a :| as
           in NE.toList $ NE.reverse $ attach l :| prev
   in PathExpr [] Nothing (Path b segs' s) noSpan

convertGenericArgs :: Sub.GenericArgs -> GenericArgs ()
convertGenericArgs (Sub.AngleBracketed args) =
  AngleBracketed (map convertGenericArg args) [] noSpan

convertGenericArg :: Sub.GenericArg -> GenericArg ()
convertGenericArg (Sub.TypeArg (Sub.RustType ty)) = TypeArg ty

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
