{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}

module DFLoweringSpec where



import Ohua.Core.Prelude

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Test.Hspec

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Passes
import qualified Ohua.Core.InternalFunctions as IFuns
import Ohua.Core.DFGraph
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.PPrint
import Ohua.Core.DFLang.Passes
import Ohua.Core.Test (embedALang, embedDFLang, showWithPretty)
import Ohua.Core.Test.DFGraph

shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

runLowering :: Expression -> IO DFExpr
runLowering =
    fmap (either error id) .
    runSilentLoggingT .
    runFromExpr
        def
        (lowerALang <=<
         normalize <=< Ohua.Core.ALang.Passes.runCorePasses <=< normalize)

-- | IMPORTANT: Both source and target expression must be in SSA form
shouldLowerTo :: Expression -> DFExpr -> Expectation
shouldLowerTo input expected = do
    lowered <- runLowering input
    -- traceM $ "expected:\n" <> (show $ prettyDFExpr expected)
    -- traceM $ "got:\n" <> (show $ prettyDFExpr lowered)
    showWithPretty lowered `shouldBe` showWithPretty expected
    -- let gr1 = (toFGLGraph . toGraph) lowered
    -- let gr2 = toFGLGraph $ toGraph expected
    -- (matchAndReport `on` unGr) gr1 gr2

lowerAndValidate :: Expression -> DFExpr -> String -> Spec
lowerAndValidate sourceExpr targetExpr statementType = do
    it ("correctly lowers an " ++ statementType ++ " statement") $
        sourceExpr `shouldLowerTo` targetExpr

smapLowering :: Spec
smapLowering =
    describe "lowering smap constructs" $ do
        let sourceExpr =
                [embedALang|
                  let coll = ohua.lang/id 0 in
                  let x = ohua.lang/smap
                              (\y -> let z = some.module/inc y in z)
                              coll in
                    x
                 |]
        let targetExpr =
                [embedDFLang|
                  let (coll) = ohua.lang/id<1> (0)  in
                  let (ctrls_0) = dataflow ohua.lang/smapFun<2> (coll)  in
                  let (d_0) = ohua.lang/nth<3> (0, 3, ctrls_0)  in
                  let (ctrl_0) = ohua.lang/nth<4> (1, 3, ctrls_0)  in
                  let (size_0) = ohua.lang/nth<5> (2, 3, ctrls_0)  in
                  let (z) = some.module/inc<6> (d_0)  in
                  let (resultList_0) = dataflow ohua.lang/collect<7> (size_0, z)  in
                    resultList_0
                |]
        lowerAndValidate sourceExpr targetExpr "smap"

smapSpec :: Spec
smapSpec = smapLowering

ifLowering :: Spec
ifLowering =
    describe "lowering conditionals" $ do
        let sourceExpr =
                [embedALang|
                  let a = ohua.lang/id 0 in
                  let b = ohua.lang/id 1 in
                  let c = ohua.lang/id 2 in
                  let z = ohua.lang/if
                            c
                           (\() -> let f = someNs/plus a b in f)
                           (\() -> let f0 = someNs/minus a b in f0) in
                    z
                |]
        let targetExpr =
                [embedDFLang|
                  let (a) = ohua.lang/id<1> (0)  in
                  let (b) = ohua.lang/id<2> (1)  in
                  let (c) = ohua.lang/id<3> (2)  in
                  let (ctrls_0) = ohua.lang/ifFun<4> (c)  in
                  let (ctrlTrue_0) = ohua.lang/nth<5> (0, 2, ctrls_0)  in
                  let (ctrlFalse_0) = ohua.lang/nth<6> (1, 2, ctrls_0)  in
                  let (ctrl_0) = dataflow ohua.lang/ctrl<7> (ctrlTrue_0, a, b)  in
                  let (a_0) = ohua.lang/nth<8> (0, 2, ctrl_0)  in
                  let (b_0) = ohua.lang/nth<9> (1, 2, ctrl_0)  in
                  let (f) = someNs/plus<10> (a_0, b_0)  in
                  let (ctrl_1) = dataflow ohua.lang/ctrl<11> (ctrlFalse_0, a, b)  in
                  let (a_1) = ohua.lang/nth<12> (0, 2, ctrl_1)  in
                  let (b_1) = ohua.lang/nth<13> (1, 2, ctrl_1)  in
                  let (f0) = someNs/minus<14> (a_1, b_1)  in
                  let (result_0) = dataflow ohua.lang/select<15> (c, f, f0)  in
                  result_0
                |]
        lowerAndValidate sourceExpr targetExpr "if"

generalLowering :: Spec
generalLowering = do
    describe "lowering a stateful function" $ do
        it "lowers a function with one argument" $
            [embedALang|
              let a = ohua.lang/id 0 in
              let x = some/function a in
                x
              |] `shouldLowerTo`
            [embedDFLang|
              let (a) = ohua.lang/id<1> (0) in
              let (x) = some/function<2> (a) in
                x
            |]
        it "lowers a function with one env argument" $
            [embedALang| let x = some/function 0 in x |] `shouldLowerTo`
            [embedDFLang| let (x) = some/function<1> (0) in x |]
        -- left out for the moment one we merge the unit stuff into the core we can add it back
        -- it "lowers a function with no arguments" $
        --     Let "x" "some/function" "x"
        --     `shouldLowerTo`
        --     DFExpr [ LetExpr 0 "x" "some/function" [] Nothing ] "x"

ifSpec :: Spec
ifSpec = ifLowering

instance IsString a => IsString (Maybe a) where
    fromString = Just . fromString

seqSpec :: Spec
seqSpec = do
    describe "seq lowering" $ do
        it
            "lowers a seq with a lambda and an independent function with a integer literal as input" $
            [embedALang|
              let y = ohua.lang/id 0 in
              let x = ohua.lang/seq y (\() -> let p = some/function 1 in p) in
                x
            |] `shouldLowerTo`
            [embedDFLang|
              let (y) = ohua.lang/id<1> (0)  in
              let (ctrl_0) = ohua.lang/seqFun<2> (y)  in
              let (ctrl_1) = dataflow ohua.lang/ctrl<3> (ctrl_0, 1)  in
              let (lit_1_0) = ohua.lang/nth<4> (0, 1, ctrl_1)  in
              let (p) = some/function<5> (lit_1_0)  in
                p
            |]
        -- Test case should be fixed with with #30
        it
            "lowers a seq with a lambda and an independent function with a unit as input" $
            [embedALang|
                let y = ohua.lang/id 0 in
                let x = ohua.lang/seq y (\() -> let p = some/function () in p) in
                  x
              |] `shouldLowerTo`
            [embedDFLang|
                 let (y) = ohua.lang/id<1> (0) in
                 let (ctrl_0) = ohua.lang/seqFun<2> (y) in
                 let (ctrl_1) = dataflow ohua.lang/ctrl<3> (ctrl_0, ()) in
                 let (lit_unit_0) = ohua.lang/nth<4> (0, 1, ctrl_1) in
                 let (p) = ohua.lang/unitFn<5> (some/function, lit_unit_0) in
                 p
            |]

matchAndReport :: (Eq a, Ord b, Show a, Show b) => Gr a b -> Gr a b -> IO ()
matchAndReport gr1 gr2
      -- TODO add a check here to verify that all nodes have unique function IDs
--matchAndReport g1 g2 =
--    let gr1 = trace ("Graph #1: " ++ show g1) g1
--        gr2 = trace ("Graph #2: " ++ show g2) g2 in
 =
    case matchGraph gr1 gr2 of
        Right _ -> return ()
        Left (largest, keys0) ->
            let selectedGr1Nodes = IntMap.elems largest
                selectedGr2Nodes = IntMap.keys largest
                unselectedGr1Nodes =
                    filter
                        (not .
                         flip IntSet.member (IntSet.fromList selectedGr1Nodes) .
                         fst)
                        (labNodes gr1)
                unselectedGr2Nodes =
                    filter
                        (not . flip IntSet.member (IntMap.keysSet largest) . fst)
                        (labNodes gr2)
             in expectationFailure $
                toString $
                unlines
                    [ "Graphs weren't isomorphic."
                    , "The largest match was between"
                    , ""
                    , prettify0 (subgraph selectedGr1Nodes gr1)
                    , ""
                    , "and"
                    , ""
                    , prettify0 (subgraph selectedGr2Nodes gr2)
                    , ""
                    , "I could not match the nodes"
                    , ""
                    , show $ unselectedGr1Nodes
                    , ""
                    , "with"
                    , ""
                    , show $ unselectedGr2Nodes
                    , case keys0 of
                          Nothing -> ""
                          Just (_, x) ->
                              unlines
                                  [ ""
                                  , "I failed when trying to match"
                                  , show $
                                    filter ((== x) . fst) unselectedGr1Nodes
                                  , "with the edges:"
                                  , show $
                                    filter
                                        (\(a, b, _) -> a == x || b == x)
                                        (labEdges gr1)
                                  , "With any of: "
                                  , show unselectedGr2Nodes
                                  ]
                    ]
  where
    prettify0 = toText . prettify

spec :: Spec
spec = do
    smapSpec
    ifSpec
    generalLowering
    seqSpec
