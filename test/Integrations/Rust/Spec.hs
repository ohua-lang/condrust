module Integrations.Rust.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec
import qualified Integrations.Rust.Basic as Basic
import qualified Integrations.Rust.If as If


spec :: Spec
spec = 
    Basic.spec >> 
    If.spec