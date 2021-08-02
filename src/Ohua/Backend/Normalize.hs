module Ohua.Backend.Normalize where

import Ohua.Prelude
import Ohua.Backend.Types
import Ohua.Backend.Lang



normalize :: Namespace (TCProgram chan recv (TaskExpr ty)) anno
          -> Namespace (TCProgram chan recv (TaskExpr ty)) anno
normalize = updateTaskExprs normalizeTaskExpr

normalizeTaskExpr :: TaskExpr ty -> TaskExpr ty
normalizeTaskExpr = normalizeLits . normalizeIndirect

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
normalizeIndirect = rewrite go
  where
    go (Let x y@Var{} ct) = Just $ substitute (x,y) ct
    go _ = Nothing

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
normalizeLits = transform go
  where
    go (Let bnd l@Lit{} ct) = substitute (bnd,l) ct
    go e = e

-- |
-- Substitution function: [x |-> y]t
substitute :: (Binding, TaskExpr ty) -> TaskExpr ty -> TaskExpr ty
substitute (bnd, e) = transform go
  where
    go (Var b) | b == bnd = e
    go e' = e'
