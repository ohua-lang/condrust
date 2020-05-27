-- |
-- Module      : $Header$
-- Description : Tests for lowering the frontend language into ALang.
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file

import Ohua.Prelude

-- import qualified Data.Text as T
import Test.Hspec

import Ohua.Frontend.Lang
import Ohua.Frontend.Lower
import Ohua.ALang.Lang
import qualified Ohua.ALang.Refs as ALangRefs

import qualified Data.HashSet as HS


main :: IO ()
main = hspec $
    describe "removing destructuring" $ do
        let mkNth0 objBnd i total =
                PureFunction ALangRefs.nth Nothing `Apply` Lit (NumericLit i) `Apply`
                Lit (NumericLit total) `Apply`
                Var objBnd
            runRemDestr = 
                (either (error . show) id <$>) . 
                runExceptT . 
                runGenBndT (HS.fromList ["a", "b", "c"]) . 
                toAlang
        it "removes destructuring from lets" $
            let objBnd = "d"
                mkNth = mkNth0 objBnd
             in runRemDestr (LetE ["a", "b", "c"] "x" "y") -- [embedALang| let (a, b, c) = x in y |]
                `shouldReturn`
                Let
                    objBnd
                    "x"
                    (Let "a" (mkNth 0 3) $
                     Let "b" (mkNth 1 3) $ Let "c" (mkNth 2 3) "y")
        it "removes destructuring from lambdas" $
            let objBnd = "d"
                mkNth = mkNth0 objBnd
             in runRemDestr (LamE [["a", "b", "c"]] "y") -- [embedALang| \(a, b, c) -> y |] 
                `shouldReturn`
                Lambda
                    objBnd
                    (Let "a" (mkNth 0 3) $
                     Let "b" (mkNth 1 3) $ Let "c" (mkNth 2 3) "y")
