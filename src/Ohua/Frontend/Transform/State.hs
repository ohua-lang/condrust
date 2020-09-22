module Ohua.Frontend.Transform.State where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Data.HashMap.Lazy as HM 


check :: CompM m => Expr -> m ()
check exp = void $ evalStateT (transformM f exp) HM.empty
    where
        f :: CompM m => Expr -> StateT (HM.HashMap Binding Int) m Expr
        f e@(BindE (VarE bnd) _) = modify (HM.insertWith (+) bnd 1) >> return e
        f e@(LetE p _ _) = clearDefined p >> return e
        f e@(LamE ps _) = clearDefined (TupP ps) >> return e
        f e@(MapE _ _)= checkAndFail e >> return e
        f e@IfE{} = checkAndFail e >> return e
        f e = return e

        clearDefined :: CompM m => Pat -> StateT (HM.HashMap Binding Int) m ()
        clearDefined p =
            mapM_ (modify . HM.delete) [bnd | VarP bnd <- universe p]

        checkAndFail :: CompM m => Expr -> StateT (HM.HashMap Binding Int) m ()
        checkAndFail e = do
            reused <- HM.keys . HM.filter (>1) <$> get
            mapM_ (lift . fail e) reused

        fail :: CompM m => Expr -> Binding -> m ()
        fail e v = throwError $ "State variable" <> show v <> "used more than once in expression:\n" <> show e