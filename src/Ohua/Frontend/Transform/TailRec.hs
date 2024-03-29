module Ohua.Frontend.Transform.TailRec where

import Ohua.Commons.Prelude hiding (Type)

import Ohua.Frontend.Lang
import Ohua.Frontend.Transform.Scopedness (contextedTraversal)

import Data.HashSet as HS 

-- This module needs to hold all the code for detecting and validating
-- the tail recursion that we currently support. (see ohua-core/#10)

isRec :: Binding -> UnresolvedExpr embExpr ty -> Bool
isRec currentFun expr = execState (contextedTraversal check HS.empty expr) False
  where
    check _ v@(VarE bdg _ty) = modify (\c -> c || bdg == currentFun) >> return v
    check _ e = return e

isRecAlgo :: Algo (UnresolvedExpr embExpr ty) a (OhuaType ty 'Resolved) -> Bool
isRecAlgo (Algo aName _aType code _ ) = isRec aName code
