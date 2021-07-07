module Integrations.Python.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Python.Basic as Basic


spec :: Spec
spec = 
    Basic.spec 

