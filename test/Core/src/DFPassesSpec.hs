module DFPassesSpec where

import Ohua.Core.Prelude
import Ohua.Core.Test
import Test.Hspec

import qualified Ohua.Core.InternalFunctions as Refs
import Ohua.Core.DFLang.Passes (collapseNth)
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.PPrint ()


collapseNthSpec :: Spec
collapseNthSpec = describe "collapseNth" $ do
    it "for if" $ do
        let trans = showWithPretty . collapseNth (== nodeRef IFuns.ifFun)
        trans
            [embedDFLang|
                let (a) = ohua.lang/ifFun<0>(cond) in
                let (a0) = ohua.lang/nth<1>(0, 2, a) in
                let (a1) = ohua.lang/nth<2>(1, 2, a) in y
                    |]
            `shouldBe`
            showWithPretty
            [embedDFLang|
                let (a0, a1) = dataflow ohua.lang/ifFun<0>(cond) in y
                        |]
    it "for smap" $ do
        let trans = showWithPretty . collapseNth (== nodeRef IFuns.smapFun)
        trans
            [embedDFLang|
                let (a) = ohua.lang/smapFun<0>(coll) in
                let (d) = ohua.lang/nth<1>(0, 3, a) in
                let (ctrl) = ohua.lang/nth<2>(1, 3, a) in
                let (size) = ohua.lang/nth<3>(2, 3, a) in y
                        |]
            `shouldBe`
            showWithPretty
            [embedDFLang|
                let (d, ctrl, size) = dataflow ohua.lang/smapFun<0>(coll) in
                y
                        |]

spec = collapseNthSpec
