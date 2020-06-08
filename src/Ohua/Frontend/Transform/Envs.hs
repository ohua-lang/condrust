module Ohua.Frontend.Transform.Envs where

import Ohua.Prelude

import Ohua.Frontend.Lang


prepareRootAlgoVars :: CompM m => Expr -> m Expr
prepareRootAlgoVars (LamE vars body) =  go 0 vars body
  where
    go i (x:xs) rest =
        go (i+1) xs $ LetE x (LitE $ EnvRefLit $ makeThrow i) rest
    go _ [] rest = return rest
prepareRootAlgoVars _ = throwError "compiler invariant broken"
