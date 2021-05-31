module Ohua.Types.Unit where

import Universum

-- TODO: Integrate with definition from Ohua.Types.Literal.Lit
data Unit = Unit
    deriving (Show, Eq, Generic)

instance Hashable (Unit)
