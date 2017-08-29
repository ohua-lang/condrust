module Ohua.ALang.NS where


import Ohua.Types
import Ohua.ALang.Lang
import qualified Data.HashMap.Strict as HM


-- | A namespace as defined by the ohua API. It has a name, a list of dependencies and aliasings, 
-- defined expressions (currently constrained to lambdas/algos) and optionally ends with an executable expression.
data Namespace = Namespace Binding [(Binding, [Binding])] (HM.HashMap Binding Expression) (Maybe Expression)

