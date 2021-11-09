module Ohua.Backend.Normalize where

import qualified Data.HashSet as HS
import Ohua.Backend.Lang
import Ohua.Backend.Types
import Ohua.Prelude hiding (First, Second)

normalize ::
  Namespace (TCProgram chan recv (TaskExpr ty)) anno ->
  Namespace (TCProgram chan recv (TaskExpr ty)) anno
normalize = updateTaskExprs normalizeTaskExpr

normalizeTaskExpr :: TaskExpr ty -> TaskExpr ty
normalizeTaskExpr = normalizeLits . normalizeIndirect

-- TODO(feliix42): The name no longer makes sense since we are indeed renaming state variables :D
transformNoState :: (TaskExpr ty -> TaskExpr ty) -> TaskExpr ty -> TaskExpr ty
transformNoState f = (`evalState` HS.empty) . transformM go
  where
    go e@(Apply (Stateful (Var v) _ _)) = do
      return $ f e
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
      -- FIXME (Sebastian): This is a hack. I do not understand the problem here. Please explain and open an issue.
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

    go (Apply (Stateless fn args)) = Apply $ Stateless fn $ map (transform go) args
    go (Apply (Stateful state fn args)) =
      Apply $ Stateful (transform go state) fn $ map (transform go) args

    go (SendData (SSend chan (Left sbnd))) | sbnd == bnd = case e of
      -- Also rename outbound variables _if_ we replace with a variable. If replacing with something else we'd need to think about another solution
      (Var newBnd) -> SendData $ SSend chan $ Left newBnd
      -- TODO(feliix42): I'm not 100% positive on keeping that error here
      (Lit (FunRefLit _)) -> error $ "Internal error: Cannot substitute " <> show bnd <> " with a function reference: " <> show e
      (Lit l) -> SendData $ SSend chan $ Right l

    go (ForEach item iterator body) | iterator == bnd = ForEach item (replaceWhenVar iterator e) $ transform go body
    go (HasSize sbnd) | sbnd == bnd = HasSize $ replaceWhenVar sbnd e
    go (Size sbnd) | sbnd == bnd = Size $ replaceWhenVar sbnd e
    go (ListOp (Append b e)) = ListOp $ Append b $ transform go e
    go (Tuple fst snd) = Tuple (replaceEither fst) (replaceEither snd)
    go (First sbnd) | sbnd == bnd = First $ replaceWhenVar sbnd e
    go (Second sbnd) | sbnd == bnd = Second $ replaceWhenVar sbnd e
    go (Increment sbnd) | sbnd == bnd = Increment $ replaceWhenVar sbnd e
    go (Decrement sbnd) | sbnd == bnd = Decrement $ replaceWhenVar sbnd e
    go e' = e'

    replaceEither (Left b) | b == bnd = case e of
                               (Var newBnd) -> Left newBnd
                               -- NOTE(feliix42): Not sure if it makes sense to check for `FunRefLit` here
                               (Lit l) -> Right l
    replaceEither e' = e'

    replaceWhenVar :: Binding -> TaskExpr ty -> Binding
    replaceWhenVar b (Var newBnd) = newBnd
    replaceWhenVar b _ = b

    updateVar (Var b) | b == bnd = e
    updateVar v = v
