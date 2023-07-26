module Ohua.Frontend.Transform.Envs where

import Ohua.UResPrelude

import Ohua.Frontend.Lang

-- FIXME this should just all go away! it would if we were compiling the whole lambda expression and not just the body of it!
-- | Transform argument patterns to let bindings i.e. @\ x y -> function_body@ becomes @let x = $x in let y = $ y in function_body@
--   for all algos

-- FIXME: Change res to 'Resolved probably
prepareRootAlgoVars :: ErrAndLogM m => Expr ty 'Resolved -> m (Expr ty 'Resolved)
prepareRootAlgoVars (LamE vars body) =  go (toList vars) body
  where
    go ((VarP x xty):xs) rest =
        go xs $ LetE (VarP x xty) (LitE $ EnvRefLit x xty) rest
    go [] rest = return rest
prepareRootAlgoVars _ = throwError "Error: Algo has none-var pattern arguments."
