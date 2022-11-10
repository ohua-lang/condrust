{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.CodeSamples.BenchInput where

import Integrations.Python.CodeSamples.SimpleQuoter

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
