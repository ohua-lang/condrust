module Ohua.Frontend.Transform.FinalLiterals where

import Ohua.UResPrelude

import Ohua.Frontend.Lang

noFinalLiterals :: (ErrAndLogM m) => Expr ty -> m ()
noFinalLiterals = go
  where
    go (VarE _ _) = return ()
    go (LitE _) = return ()
    go (LetE _p _f l@(LitE FunRefLit{} )) =
      throwError $ "It seems you tried to have a function ("<> show l <>" ) as return value. The compiler will not handle this."
    go (LetE _p _f l@LitE{}) = errorOut l
    go (LetE p f cont) = go f >> go cont
    go (AppE e1 e2) = mapM_ go (e1:e2)
    go (LamE _ e) = go e
    go (IfE e1 e2 e3) = go e1 >> go e2 >> go e3
    go (WhileE e1 e2) = go e1 >> go e2
    go (MapE e1 e2) = go e1 >> go e2
    go (BindE e1 e2) = go e1 >> go e2
    go (StmtE f l@LitE{}) = errorOut l
    go (StmtE f cont) = go f >> go cont
    go (TupE es) = mapM_ go es

    errorOut l = throwError $ "The literal [" <> show l <> "] does not depend on a previously computed value. In the host program this meant that it depends on the previous statements. This compiler does not yet support creating this dependency implicity. Hence, we kindly ask you to add it explicitly to your code."

