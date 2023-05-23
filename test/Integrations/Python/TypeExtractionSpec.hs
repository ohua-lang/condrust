{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TypeExtractionSpec
    ( spec
    ) where


import Ohua.Prelude

import Test.Hspec

import qualified Language.Python.Common.AST as Py

import qualified Data.HashMap.Lazy as HM

import Ohua.Integration.Python.TypeExtraction

{-- TODO:   1. Where to put the Quasiquoter to avoid redundancy?
            2. Write testcase python to check type extraction from functions
             probably important: correct binding, number of arguments, 'typed/untyped', 'self' argument
                                'mutable/immutable' (only when typed, otherwise probably anything is considered mutable),
                                 positional args, kwargs 
            --}

-- extractTypes :: Show a => Py.Module a -> IO (HM.HashMap QualifiedBinding (FunType PythonVarType))
-- extractTypes srcFile = runErrAndLogM LevelDebug $ extract "" srcFile

spec :: Spec
spec = 
    describe "type extraction" $ do
        it "top-level functions" $
            1 `shouldBe` 1
