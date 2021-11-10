{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.BenchInput where

import Integrations.Python.SimpleQuoter

blackScholes = [pythonModule|
from bs_lib import calulateForOption, refl

def algo(ops):
    valOps = refl(ops)
    results = list()
    for op in valOps:
        n = calulateForOption(op)
        results.append(n)
    return results
|]


