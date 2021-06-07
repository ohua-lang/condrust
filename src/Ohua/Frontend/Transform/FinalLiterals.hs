module Ohua.Frontend.Transform.FinalLiterals where

import Ohua.Prelude

import Ohua.Frontend.Lang

-- import Data.Functor.Foldable (cata, embed)

transformFinalLiterals :: Expr ty -> Expr ty
  -- TODO I believe, we want to raise an error if this is a function literal.
  --      But I also believe that it is about time to differentiate between function literals
  --      and the rest of the literal types.
  -- cata $ \case
  --  StmtEF e u@(LitE _) -> SeqE e u
  --  e -> embed e
transformFinalLiterals = \case
  (LamE v e) -> LamE v $ go e
  e -> e -- FIXME really this case should not be here! type too weak!
  where
    go (LetE p f l@LitE{}) = constLit f l
    go (LetE p f cont) = LetE p f $ go cont
    go (StmtE f l@LitE{}) = constLit f l
    go (StmtE f cont) = StmtE f $ go cont
    go e = e
    constLit f l =
      LetE (VarP "x") f $
      SeqE (VarE "x") l

