module Ohua.Integration.Rust.Backend where

import Data.Functor.Foldable (cata, embed)
import qualified Data.HashMap.Lazy as HM
import Data.List ((!!))
import Data.Maybe
import Language.Rust.Parser (Span)
import Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Rust.Backend.Convert (prependToBlock)
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import Ohua.Integration.Rust.TypeExtraction as TE (RustArgType (Normal), RustTypeAnno)
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util
import Ohua.Prelude
import Ohua.Types.Vector (Nat (..))

convertIntoBlock ::
  (Architecture arch, Lang arch ~ Language 'Rust) =>
  arch ->
  TaskExpr RustTypeAnno ->
  Sub.Block
convertIntoBlock arch expr =
  let expr' = convertExpr arch expr
   in case expr' of
        Sub.BlockExpr block -> block
        e -> Sub.Block [Sub.NoSemi e]

instance Integration (Language 'Rust) where
  type NS (Language 'Rust) = Module
  type Type (Language 'Rust) = RustTypeAnno
  type AlgoSrc (Language 'Rust) = Item Span

  type Expr (Language 'Rust) = Sub.Expr
  type Task (Language 'Rust) = Sub.Block

  lower (Module _path (SourceFile _ _ items)) arch ns =
    return $
      ns & algos %~ map (\algo -> algo & algoCode %~ convertTasks (algo ^. algoAnno))
    where
      convertTasks (Fn _ _ _ (FnDecl args typ _ span) _ _ block _) (Program chans (SRecv _ c) tasks) =
        Program
          chans
          (SRecv (Type $ TE.Normal $ fromMaybe (TupTy [] span) typ) c)
          $ map (convertIntoBlock arch . convertEnvs <$>) tasks

      convertEnvs :: TCLang.TaskExpr RustTypeAnno -> TCLang.TaskExpr RustTypeAnno
      convertEnvs = cata $ \case
        LitF (EnvRefLit arg) -> Var arg
        e -> embed e

  convertExpr _ (TCLang.Var b) = Sub.Var b
  convertExpr _ (TCLang.Lit l) = Sub.Lit l
  convertExpr arch (Apply (Stateless bnd args)) = convertFunCall arch bnd args
  convertExpr arch (Apply (Stateful stateExpr f args)) =
    Sub.MethodCall
      (convertExpr arch stateExpr)
      (Sub.CallRef f Nothing)
      (map (convertExpr arch) args)
  convertExpr arch (Let bnd stmt cont) =
    let stmtExpr =
          Sub.Local
            (Sub.IdentP $ Sub.IdentPat Sub.Immutable bnd)
            (convertExpr arch stmt)
        contExpr = convertExpr arch cont
     in prependToBlock [stmtExpr] contExpr
  convertExpr arch (TCLang.Stmt stmt cont) =
    prependToBlock [Sub.Semi (convertExpr arch stmt)] $ convertExpr arch cont
  convertExpr arch (TCLang.Assign bnd expr) =
    prependToBlock
      [Sub.Semi (Sub.Assign (convertExpr arch $ TCLang.Var bnd) (convertExpr arch expr))]
      $ convertExpr arch $ TCLang.Lit UnitLit
  convertExpr arch (ReceiveData recv) = convertRecv arch recv
  convertExpr arch (SendData send) = convertSend arch send
  convertExpr arch (TCLang.EndlessLoop expr) =
    Sub.Loop $ convertIntoBlock arch expr
  convertExpr arch (TCLang.ForEach bnd colBnd expr) =
    let block = convertIntoBlock arch expr
        colExpr = convertExpr arch $ Var colBnd
        loopVar = Sub.IdentPat Sub.Immutable bnd
     in Sub.ForLoop loopVar colExpr block
  convertExpr arch (TCLang.Repeat count expr) =
    let loopVar = Sub.IdentPat Sub.Immutable "_"
        repetition = case count of
          Left bnd -> convertExpr arch $ Var bnd
          Right cnt -> convertExpr arch (TCLang.Lit $ NumericLit $ fromIntegral cnt)
        range = Sub.HalfOpenRange (Just $ convertExpr arch $ TCLang.Lit $ NumericLit 0) (Just repetition)
        block = convertIntoBlock arch expr
     in Sub.ForLoop loopVar range block
  convertExpr arch (TCLang.While loopHead body) =
    Sub.While (convertExpr arch loopHead) (convertIntoBlock arch body)
  convertExpr arch (TCLang.Cond condExpr trueBranch falseBranch) =
    Sub.If
      (convertExpr arch condExpr)
      (convertIntoBlock arch trueBranch)
      (Just $ convertExpr arch falseBranch)
  convertExpr arch (TCLang.ListOp Create) = convertExpr arch $ Apply $ Stateless "Vec/new" []
  convertExpr arch (TCLang.ListOp (Append bnd expr)) =
    convertExpr arch $ Apply $ Stateful (Var bnd) (mkFunRefUnqual "push") [expr]
  convertExpr arch (TCLang.Size bnd) =
    convertExpr arch $ Apply $ Stateful (Var bnd) (mkFunRefUnqual "len") []
  convertExpr arch (TCLang.Tuple one two) =
    let conv = convertExpr arch . either TCLang.Var TCLang.Lit
     in Sub.Tuple [conv one, conv two]
  convertExpr arch (TCLang.First bnd) = Sub.TupleField (convertExpr arch $ Var bnd) Zero
  convertExpr arch (TCLang.Second bnd) = Sub.TupleField (convertExpr arch $ Var bnd) $ Succ Zero
  convertExpr arch (TCLang.Increment bnd) =
    convertExpr arch $
      Apply $ Stateless (mkFunRefUnqual "+") [Var bnd, TCLang.Lit $ NumericLit 1]
  convertExpr arch (TCLang.Decrement bnd) =
    convertExpr arch $
      Apply $ Stateless (mkFunRefUnqual "-") [Var bnd, TCLang.Lit $ NumericLit 1]
  convertExpr arch (TCLang.Not expr) = convertExpr arch $ Apply $ Stateless (mkFunRefUnqual "!") [expr]
  convertExpr arch (TCLang.HasSize bnd) =
    let intermediate = toBinding "tmp_has_size"
     in convertExpr arch $ Let intermediate (Apply $ Stateful (Apply $ Stateful (Var bnd) (mkFunRefUnqual "iter") []) (mkFunRefUnqual "size_hint") []) (Apply $ Stateful (Second intermediate) (mkFunRefUnqual "is_some") [])

-- In the old runtime this was implemented using the `size_hint` function in Rust
-- What I want here:
--      v.iter().size_hint().1.is_some()

pattern UnqualFun :: Binding -> QualifiedBinding
pattern UnqualFun bnd <- QualifiedBinding [] bnd

mkFunRefUnqual :: Binding -> QualifiedBinding
mkFunRefUnqual = QualifiedBinding (makeThrow [])

-- TODO we probably want a Literal for common operations
convertFunCall ::
  (Architecture arch, Lang arch ~ Language 'Rust, ty ~ B.Type (Lang arch)) =>
  arch ->
  QualifiedBinding ->
  [TCLang.TaskExpr RustTypeAnno] ->
  Sub.Expr
convertFunCall arch f args =
  case (binOp f, args) of
    (Just bOp, [arg1, arg2]) -> Sub.Binary bOp (convertExpr arch arg1) (convertExpr arch arg2)
    _ -> case (unOp f, args) of
      (Just uOp, [arg]) -> Sub.Unary uOp (convertExpr arch arg)
      _ ->
        Sub.Call (Sub.CallRef f Nothing) $ map (convertExpr arch) args
  where
    binOp = \case
      UnqualFun "+" -> Just Sub.Add
      UnqualFun "-" -> Just Sub.Sub
      UnqualFun "*" -> Just Sub.Mul
      UnqualFun "/" -> Just Sub.Div
      _ -> Nothing
    unOp = \case
      UnqualFun "!" -> Just Sub.Not
      UnqualFun "-" -> Just Sub.Neg
      UnqualFun "*" -> Just Sub.Deref
      _ -> Nothing
