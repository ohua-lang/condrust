module Ohua.Frontend.Transform.FinalUnit where

import Ohua.Prelude

import Ohua.Frontend.Lang

import Data.Functor.Foldable (cata, embed)

transformFinalUnit :: Expr -> Expr
transformFinalUnit = cata $ \case
    StmtEF e u@(LitE UnitLit) -> SeqE e u
    e -> embed e
