module Ohua.Frontend.Transform.FinalLiterals where

import Ohua.Prelude

import Ohua.Frontend.Lang

-- QUESTION: I don't understand what this step is supposed to do and couldn't figure out by debug testing what it does.

transformFinalLiterals :: Expr ty -> Expr ty

transformFinalLiterals = \case
  (LamE v e) -> LamE v $ go e
  e -> e -- FIXME really this case should not be here! type too weak!
  where
    -- Question: There was a todo saying we should error upon returning a function literal. But is it really 'our problem'?
    go (LetE _p f l@(LitE FunRefLit{} )) = error $ "It seems you tried to have a function ("<> show l <>" ) as return value. The compiler will not handle this."
    go (LetE _p f l@LitE{}) = constLit f l
    go (LetE p f cont) = LetE p f $ go cont
    go (StmtE f l@LitE{}) = constLit f l
    go (StmtE f cont) = StmtE f $ go cont
    go e = e
    constLit f l =
      let ty = exprType f in
          LetE (VarP "x" ty) f $
          SeqE (VarE "x" ty) l

