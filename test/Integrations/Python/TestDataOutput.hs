{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataOutput where

import Integrations.Python.SimpleQuoter


-- Test cases for Basic.hs
callAFunctionOut = [pythonModule|
from testLib import hello_world

def calling():
    hello_world()
|]