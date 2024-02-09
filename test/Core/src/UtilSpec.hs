module UtilSpec where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Util
import Ohua.Core.Test
import Ohua.Core.ALang.Lang

import Test.Hspec


spec :: Spec
spec = do
    let doit e = findLonelyLiterals e
    describe "findLonelyLiterals" $ do
        it "finds a single argument literal" $
            doit [embedALang| a/b 1 |] `shouldBe` [Lit $ NumericLit 1]
        it "ignores functions with var input" $ do
            doit [embedALang| a/b t 4 |] `shouldBe` []
            doit [embedALang| a/b 4 t |] `shouldBe` []
