module Ohua.Backend.Normalize where

import qualified Data.HashSet as HS
import Ohua.Backend.Lang
import Ohua.Backend.Types
import Ohua.Prelude hiding (First, Second)

normalize ::
  Namespace (Program chan recv (TaskExpr embExpr ty) embExpr ty) anno (OhuaType ty 'Resolved) ->
  Namespace (Program chan recv (TaskExpr embExpr ty) embExpr ty) anno (OhuaType ty 'Resolved)
normalize = updateTaskExprs' normalizeTaskExpr

normalizeTaskExpr :: TaskExpr embExpr ty -> TaskExpr embExpr ty
normalizeTaskExpr = normalizeLits . normalizeIndirect

transformWithState :: (TaskExpr embExpr ty -> TaskExpr embExpr ty) -> TaskExpr embExpr ty -> TaskExpr embExpr ty
transformWithState f = (`evalState` HS.empty) . transformM go
  where
    -- This restricts the whole transformation to stateful functions only! Why?!
    go e@(Apply (Stateful (Var v) _ _)) = do
      return $ f e
    go e@(Assign b _) = do
      modify $ HS.insert b
      return e
    go e@(Let x _ _) = do
      states <- get
      if HS.member x states then (do
        put $ HS.delete x states
        return e) else return $ f e
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
normalizeIndirect :: TaskExpr embExpr ty -> TaskExpr embExpr ty
normalizeIndirect = transformWithState go
  where
    go (Let x y@Var {} ct) =
      -- Question: This is the only place where containcBinding is used and a comment close to it's definition says this
      --           function should be "remvoved ASAP"
      --           So do we meanwhile know how we get here and why this function is necessary and so fragile at the same time?

      -- NOTE(feliix42): Don't do the substitution if a subsequent check shows remaining bindings in the Expr. This is to avoid producing invalid code.
      -- FIXME (Sebastian): This is a hack. I do not understand the problem here. Please explain and open an issue.
      let
        -- FIXME substitutation is just broken here. Just do not substitute when the var is redefined somewhere.
        newExpr = substitute (x, y) ct
      in
        if containsBinding newExpr x then 
          Let x y ct else newExpr
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
normalizeLits :: TaskExpr embExpr ty -> TaskExpr embExpr ty
normalizeLits = transformWithState go
  where
    go (Let bnd l@Lit {} ct) = substitute (bnd, l) ct
    go e = e

-- |
-- Substitution function: [x |-> y]t
substitute :: (Binding, TaskExpr embExpr ty) -> TaskExpr embExpr ty -> TaskExpr embExpr ty
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

    -- FIXME this is broken: what if item == bnd?!
    go (ForEach item iterator body) | iterator == bnd = ForEach item (replaceWhenVar iterator e) body -- ForEach item (replaceWhenVar iterator e) $ transform go body
    go (HasSize sbnd) | sbnd == bnd = HasSize $ replaceWhenVar sbnd e
    go (Size sbnd) | sbnd == bnd = Size $ replaceWhenVar sbnd e
    go (ListOp (Append b e)) = ListOp $ Append b $ transform go e
    go (Tuple itms) = Tuple ( map replaceEither itms)
    go (Indexing sbnd num) | sbnd == bnd = Indexing (replaceWhenVar sbnd e) num
    go (Increment sbnd) | sbnd == bnd = Increment $ replaceWhenVar sbnd e
    go (Decrement sbnd) | sbnd == bnd = Decrement $ replaceWhenVar sbnd e
    go e' = e'

    replaceEither (Left b) | b == bnd = case e of
                               (Var newBnd) -> Left newBnd
                               (Lit (EnvRefLit l _)) -> Left l
                               -- NOTE(feliix42): Not sure if it makes sense to check for `FunRefLit` here
                               -- Question: What do we do here and does it make sense?
                               (Lit l) -> Right l
    replaceEither e' = e'

    replaceWhenVar :: Binding -> TaskExpr embExpr ty -> Binding
    replaceWhenVar b (Var newBnd) = newBnd
    replaceWhenVar b (Lit (EnvRefLit newBnd _ )) = newBnd
    replaceWhenVar b _ = b

    updateVar (Var b) | b == bnd = e
    updateVar v = v
