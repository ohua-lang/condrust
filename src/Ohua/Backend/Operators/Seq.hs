module Ohua.Backend.Operators.Seq where

import Ohua.Commons.Prelude

import Ohua.Backend.Lang hiding (TCExpr)


seqFun :: TaskExpr embExpr ty
seqFun = Tuple $ Right (BoolLit True):| [Right $ NumericLit 1]