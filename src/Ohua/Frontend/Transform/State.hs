module Ohua.Frontend.Transform.Calls where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Data.HashMap.Lazy as HM 


check :: CompM m => Expr -> m Expr
check e = evalStateT (transformM f e) HM.empty
    where
        f e@(BindE (VarE bnd) _) = modify (HM.insertWith + bnd 1) >> return e
        f e@(LetE p _ _) = do
            mapM_ (modify . HM.delete) [bnd | VarP bnd <- universe p]
            return e
        f e@(MapE ctxt _)= checkAndFail e
        f e@IfE{} = checkAndFail e
        f e = return e

        checkAndFail e = do
            reused <- filter ((>1) . snd) . HM.toList <$> get
            mapM_ (fail e) reused
            return e

        fail e v = throwError "State variable" <> show v <> "used more than once in expression:\n" <> show e