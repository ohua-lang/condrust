module Ohua.Backend.Operators.Seq where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (TCExpr)


seqFun :: TaskExpr ty
seqFun = Tuple (Right $ BoolLit True) (Right $ NumericLit 1)