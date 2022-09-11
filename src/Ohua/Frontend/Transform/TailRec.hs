module Ohua.Frontend.Transform.TailRec where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Lang
import Ohua.Frontend.Transform.Scopedness (contextedTraversal)

import Data.HashSet as HS 

-- This module needs to hold all the code for detecting and validating
-- the tail recursion that we currently support. (see ohua-core/#10)

isRec :: Binding -> Expr ty -> Bool
isRec currentFun expr = execState (contextedTraversal check HS.empty expr) False
  where
    check _ v@(VarE bdg) = modify (\c -> c || bdg == currentFun) >> return v
    check _ e = return e

isRecAlgo :: Algo (Expr ty) a ty -> Bool
isRecAlgo (Algo aName code _ _) = isRec aName code
