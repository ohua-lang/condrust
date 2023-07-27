module Ohua.Frontend.Transform.FinalLiterals where

import Ohua.UResPrelude

import Ohua.Frontend.Lang



noFinalLiterals :: (ErrAndLogM m) => ResolvedExpr ty -> m ()
noFinalLiterals (VarE _ _) = return ()
noFinalLiterals (LitE _) = return ()
noFinalLiterals (LetE _p _f l@(LitE FunRefLit{} )) =
  throwError $ "It seems you tried to have a function ("<> show l <>" ) as return value. The compiler will not handle this."
noFinalLiterals (LetE _p _f l@LitE{}) = throwError $ "The literal [" <> show l <> "] does not depend on a previously computed value. In the host program this meant that it depends on the previous statements. This compiler does not yet support creating this dependency implicity. Hence, we kindly ask you to add it explicitly to your code."
noFinalLiterals (LetE _p f cont) = noFinalLiterals f >> noFinalLiterals cont
noFinalLiterals (AppE e1 e2) = mapM_ noFinalLiterals (e1 : toList e2)
noFinalLiterals (LamE _ e) = noFinalLiterals e
noFinalLiterals (IfE e1 e2 e3) = noFinalLiterals e1 >> noFinalLiterals e2 >> noFinalLiterals e3
noFinalLiterals (WhileE e1 e2) = noFinalLiterals e1 >> noFinalLiterals e2
noFinalLiterals (MapE e1 e2) = noFinalLiterals e1 >> noFinalLiterals e2
noFinalLiterals (StateFunE e1 _state e2) = noFinalLiterals e1 >> concatMapM noFinalLiterals e2
noFinalLiterals (StmtE f l@LitE{}) = throwError $ "The literal [" <> show l <> "] does not depend on a previously computed value. In the host program this meant that it depends on the previous statements. This compiler does not yet support creating this dependency implicity. Hence, we kindly ask you to add it explicitly to your code."
noFinalLiterals (StmtE f cont) = noFinalLiterals f >> noFinalLiterals cont
noFinalLiterals (TupE es) = mapM_ noFinalLiterals es
