module Ohua.Frontend.Transform.Envs where

import Ohua.Prelude

import Ohua.Frontend.Lang

-- FIXME this should just all go away! it would if we were compiling the whole lambda expression and not just the body of it!
prepareRootAlgoVars :: CompM m => Expr ty -> m (Expr ty)
prepareRootAlgoVars (LamE vars body) =  go vars body
  where
    go (VarP x:xs) rest =
        go xs $ LetE (VarP x) (LitE $ EnvRefLit x) rest
    go [] rest = return rest
prepareRootAlgoVars _ = throwError "compiler invariant broken"
