module Ohua.Util
    ( intentionally_not_implemented
    , runExceptM
    ) where

import Universum

import Ohua.Types.Error


intentionally_not_implemented :: a
intentionally_not_implemented =
    error "This is intentionally not implemented, don't use it!"

runExceptM :: ExceptT Error IO a -> IO a
runExceptM c = runExceptT c >>= either error pure
