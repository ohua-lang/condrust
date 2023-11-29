module Ohua.Frontend.Transform.FinalLiterals where

import Ohua.Prelude

import Ohua.Frontend.Lang

{-
noFunLitReturns :: (ErrAndLogM m) => UnresolvedExpr ty -> m (UnresolvedExpr ty)
noFunLitReturns e = preWalkM replaceLitReturn e
  where 
      replaceLitReturn = \case 
          (LetE _p _e1 e2@(LitE FunRefLit{} )) -> throwError $ 
                "It seems you tried to have a function ("
                <> show e2 
                <>" ) as return value. The compiler will not handle this."  
          (LetE _p e1 e2@LitE{})  -> constLit e1 e2
          (StmtE e1 e2@LitE{})    -> constLit e1 e2
          e                       -> e
      constLit e1 e2 =
        LetE (VarP "x" ) e1 $
        SeqE (VarE "x") e2
-}

noFunLitReturns :: (ErrAndLogM m) => UnresolvedExpr ty -> m ()
noFunLitReturns (VarE _ _) = return ()
noFunLitReturns (LitE _) = return ()
noFunLitReturns e@(LetE _p _f l@LitE{}) = 
  throwError $ "The literal [" <> show l <> "] does not depend on a previously computed value in the expression \n" 
  <> show e
  <>"\nIn the host program this meant that it depends on the previous statements. This compiler does not yet support creating this dependency implicity. Hence, we kindly ask you to add it explicitly to your code."
noFunLitReturns (LetE _p f cont) = noFunLitReturns f >> noFunLitReturns cont
noFunLitReturns (AppE e1 e2) = mapM_ noFunLitReturns (e1 : toList e2)
noFunLitReturns (LamE _ e) = noFunLitReturns e
noFunLitReturns (IfE e1 e2 e3) = noFunLitReturns e1 >> noFunLitReturns e2 >> noFunLitReturns e3
noFunLitReturns (WhileE e1 e2) = noFunLitReturns e1 >> noFunLitReturns e2
noFunLitReturns (MapE e1 e2) = noFunLitReturns e1 >> noFunLitReturns e2
-- noFunLitReturns (BindE m _s args) = mapM_ noFunLitReturns (m: toList args) 
noFunLitReturns (StateFunE e1 _state e2) = noFunLitReturns e1 >> concatMapM noFunLitReturns e2
-- noFunLitReturns e@(StmtE _f l@LitE{}) =  throwError $ "The literal [" <> show l <> "] does not depend on a previously computed value in the expression \n" 
--   <> show e
--   <>"\nIn the host program this meant that it depends on the previous statements. This compiler does not yet support creating this dependency implicity. Hence, we kindly ask you to add it explicitly to your code."
noFunLitReturns e@(SeqE e1 e2) =  throwError "Hit Seq, please handle"
noFunLitReturns (StmtE f cont) = noFunLitReturns f >> noFunLitReturns cont
noFunLitReturns (TupE es) = mapM_ noFunLitReturns es
