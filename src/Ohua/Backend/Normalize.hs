module Ohua.Backend.Normalize where

import qualified Data.HashSet as HS
import Ohua.Backend.Lang
import Ohua.Backend.Types
import Ohua.Prelude

normalize ::
  Namespace (TCProgram chan recv (TaskExpr ty)) anno ->
  Namespace (TCProgram chan recv (TaskExpr ty)) anno
normalize = updateTaskExprs normalizeTaskExpr

normalizeTaskExpr :: TaskExpr ty -> TaskExpr ty
normalizeTaskExpr = normalizeLits . normalizeIndirect

transformNoState :: (TaskExpr ty -> TaskExpr ty) -> TaskExpr ty -> TaskExpr ty
transformNoState f = (`evalState` HS.empty) . transformM go
  where
    go e@(Apply (Stateful (Var v) _ _)) = do
      modify $ HS.insert v
      return e
    go e@(Assign b _) = do
      modify $ HS.insert b
      return e
    go e@(Let x _ _) = do
      states <- get
      case HS.member x states of
        True -> do
          put $ HS.delete x states
          return e
        False -> return $ f e
    go e = pure e

-- |
-- Normalizes this:
-- @
--   let x = y in t
-- @
-- into that:
-- @
--   [x |-> y]t
-- @
normalizeIndirect :: TaskExpr ty -> TaskExpr ty
normalizeIndirect = transformNoState go
  where
    go (Let x y@Var {} ct) =
      -- NOTE(feliix42): Don't do the substitution if a subsequent check shows remaining bindings in the Expr. This is to avoid producing invalid code.
      let
        newExpr = substitute (x, y) ct
      in
        case containsBinding newExpr x of
          True -> traceShow ("Note: Aborted a normalization of variable " <> show x <> " because I couldn't rename all occurences.") $ Let x y ct
          False -> newExpr
    go e = e

-- |
-- Normalizes this:
-- @
--   let x = 5 in t
-- @
-- into that:
-- @
--   [x |-> 5]t
-- @
-- for every literal.
normalizeLits :: TaskExpr ty -> TaskExpr ty
normalizeLits = transformNoState go
  where
    go (Let bnd l@Lit {} ct) = substitute (bnd, l) ct
    go e = e

-- |
-- Substitution function: [x |-> y]t
substitute :: (Binding, TaskExpr ty) -> TaskExpr ty -> TaskExpr ty
substitute (bnd, e) = transform go
  where
    go v@Var {} = updateVar v
    go (SendData (SSend chan sbnd)) | sbnd == bnd = case e of
      -- Also rename outbound variables _if_ we replace with a variable. If replacing with something else we'd need to think about another solution
      (Var newBnd) -> SendData $ SSend chan newBnd
      l@Lit{} -> Let "tmpBinding" l $ SendData $ SSend chan "tmpBinding"
      _ -> error $ "Internal Error: Tried running substitution on sending " <> show sbnd <> " with not yet supported task expression: " <> show e
    go (Apply (Stateless fn args)) = Apply $ Stateless fn $ map (transform go) args
    go (Apply (Stateful state fn args)) =
      Apply $ Stateful (transform go state) fn $ map (transform go) args
    go (ListOp (Append b e)) = ListOp $ Append b $ transform go e
    go e' = e'

    updateVar (Var b) | b == bnd = e
    updateVar v = v
