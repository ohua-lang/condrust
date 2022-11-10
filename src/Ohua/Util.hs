module Ohua.Util
    ( 
        runExceptM
    ) where

import Universum

import Ohua.Types.Error


runExceptM :: ExceptT Error IO a -> IO a
runExceptM c = runExceptT c >>= either error pure
