module Ohua.Frontend.Transform.Envs where

import Ohua.Prelude

import Ohua.Frontend.Lang

-- FIXME this should just all go away! it would if we were compiling the whole lambda expression and not just the body of it!
-- | Transform argument patterns to let bindings i.e. @\ x y -> function_body@ becomes @let x = $x in let y = $ y in function_body@
--   for all algos

prepareRootAlgoVars :: ErrAndLogM m => ResolvedExpr ty -> m (ResolvedExpr ty)
prepareRootAlgoVars (LamE vars body) =  go (toList vars) body
  where
    go ((VarP x xty): xs) body' =
        go xs $ LetE (VarP x xty) (LitE $ EnvRefLit x xty) body'
    go [] body' = return body'
    go _nested _body = throwError "Error: Algo arguments contain tuple pattern. Those should have been resolved already. Please file a bug"
prepareRootAlgoVars _ = throwError "Error: Algo has none-var pattern arguments."
