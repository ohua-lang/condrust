{-# LANGUAGE LambdaCase #-}

module TailRecSpec
    ( spec
    ) where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.PPrint (quickRender)
import Ohua.Core.ALang.Passes (normalize)
import Ohua.Core.Feature.TailRec.Passes.ALang
    ( findTailRecs
    , recur
    , recur_hof
    , rewriteAll
    , verifyTailRecursion
    , recurStartMarker
    )
import qualified Ohua.Core.InternalFunctions as Refs
import Ohua.Core.DFLang.Passes (lowerALang, collapseNth)
import Ohua.Core.Feature.TailRec.Passes.DFLang (recurLowering)
import Ohua.Core.DFLang.PPrint ()
import Ohua.Core.DFLang.Lang (nodeRef)

import Ohua.Core.Test (embedALang, embedDFLang, showWithPretty)

import Test.Hspec

array = "ohua.lang/array"

sf s = PureFunction s Nothing

-- -- FIXME copied from PassesSpec. put this in a test module.
-- instance Num ResolvedSymbol where
--     fromInteger = Env . fromInteger
--
-- instance Num Expression where
--     fromInteger = Var . fromInteger
--
-- -- FIXME copied from DFLowering. put this in a test module.
-- sf :: a -> AExpr bndType (Symbol a)
-- sf = Var . flip Sf Nothing
recWithExprOnTerminalBranch :: Expression
recWithExprOnTerminalBranch
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/-" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/<" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda
    --                                         "then"
    --                                         (Let "t"
    --                                              (Apply (sf IFuns.id) "p")
    --                                              "t")))
    --                               (Lambda "else" (Let "r" ("a" `Apply` "p") "r")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))
 =
    [embedALang|
        let a = (\i -> let p = math/minus i 10 in
                       let x = math/lt p 0 in
                       let c = if x then
                                            let t = ohua.lang/id p in t
                                            else let r = a p in r in
                        c
                ) in
        let y = a 95 in
          y
      |]

recWithVarOnlyOnTerminalBranch :: Expression
recWithVarOnlyOnTerminalBranch
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/-" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/<" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda "else" (Let "r" ("a" `Apply` "p") "r")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))
 =
    [embedALang|
      let a = (\i -> let p = math/minus i 10 in
                     let x = math/lt p 0 in
                     let c = ohua.lang/if x
                                          (\_1 -> p)
                                          (\_2 -> let r = a p in r) in
                      c
              ) in
      let y = a 95 in
        y
      |]

recWithExprOnRecurBranch :: Expression
recWithExprOnRecurBranch
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/-" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/<" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda "else" (Let "r" ("a" `Apply` "p") "r")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))
 =
    [embedALang|
         let a = (\i -> let p = math/minus i 10 in
                        let x = math/lt p 0 in
                        let c = ohua.lang/if x
                                             (\_1 -> p)
                                             (\_2 -> let r = a p in r) in
                         c
                 ) in
         let y = a 95 in
           y
         |]

recWithCallOnlyOnRecurBranch :: Expression
recWithCallOnlyOnRecurBranch
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/-" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/<" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda "else" ("a" `Apply` "p")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))
 =
    [embedALang|
         let a = (\i -> let p = math/minus i 10 in
                        let x = math/lt p 0 in
                        let c = ohua.lang/if x
                                             (\_1 -> p)
                                             (\_2 -> a p) in
                         c
                 ) in
         let y = a 95 in
           y
           |]

expectedRecWithExprOnTerminalBranch :: Expression
expectedRecWithExprOnTerminalBranch =
    [embedALang|
        let a = \i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = ohua.lang/ifThenElse x
                        (\_1 -> let t = ohua.lang/id p in t)
                        (\_2 -> let r = ohua.lang/recur p in r) in
            c in
        let y = a 95 in
        y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/minus" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/lt" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda
    --                                         "then"
    --                                         (Let "t"
    --                                              (Apply (sf IFuns.id) "p")
    --                                              "t")))
    --                               (Lambda
    --                                    "else"
    --                                    (Let "r" ((sf recur) `Apply` "p") "r")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithVarOnlyOnTerminalBranch :: Expression
expectedRecWithVarOnlyOnTerminalBranch =
    [embedALang|
        let a = \i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = ohua.lang/ifThenElse x
                        (\_1 -> p)
                        (\_2 -> let r = ohua.lang/recur p in r) in
            c in
        let y = a 95 in
        y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/minus" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/lt" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda
    --                                    "else"
    --                                    (Let "r" ((sf recur) `Apply` "p") "r")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithExprOnRecurBranch :: Expression
expectedRecWithExprOnRecurBranch =
    [embedALang|
        let a = \i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = ohua.lang/ifThenElse x
                        (\_1 -> p)
                        (\_2 -> let r = ohua.lang/recur p in r) in
            c in
        let y = a 95 in
        y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/minus" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/lt" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda
    --                                    "else"
    --                                    (Let "r" ((sf recur) `Apply` "p") "r")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithCallOnlyOnRecurBranch :: Expression
expectedRecWithCallOnlyOnRecurBranch =
    [embedALang|
        let m = ohua.lang/id 95 in
        let a = \i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = if x then p else a p in
            c in
        let y = a m in
        y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/minus" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/lt" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda "else" ((sf recur) `Apply` "p")))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))

notTailRecursive1 :: Expression
notTailRecursive1 =
    [embedALang|
        let a = ohua.lang/Y (\i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = ohua.lang/ifThenElse x
                        (\_1 -> p)
                        (\_2 -> let g = ohua.lang/recur p in
                                math/times10 g) in
            c) in
        let y = a 95 in
        y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/minus" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/lt" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda
    --                                    "else"
    --                                    (Let "g"
    --                                         ((sf recur) `Apply` "p")
    --                                         ("math/times10" `Apply` "g"))))
    --                          "c"))))
    --      (Let "y" ("a" `Apply` 95) "y"))

notTailRecursive2 :: Expression
notTailRecursive2 =
    [embedALang|
        let a = ohua.lang/Y (\i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = ohua.lang/ifThenElse x
                        (\_1 -> p)
                        (\_2 -> ohua.lang/recur p) in
            math/times10 c) in
        let y = a 95 in
        y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "p"
    --                (("math/minus" `Apply` "i") `Apply` 10)
    --                (Let "x"
    --                     (("math/lt" `Apply` "p") `Apply` 0)
    --                     (Let "c"
    --                          (Apply
    --                               (Apply
    --                                    (Apply (sf IFuns.ifThenElse) "x")
    --                                    (Lambda "then" "p"))
    --                               (Lambda "else" ((sf recur) `Apply` "p")))
    --                          ("math/times10" `Apply` "c")))))
    --      (Let "y" ("a" `Apply` 95) "y"))

notTailRecursive3 :: Expression
notTailRecursive3 =
    [embedALang|
               let a = ohua.lang/Y (\i ->
                   let g =
                       let p = math/minus i 10 in
                       let x = math/lt p 0 in
                       let c = ohua.lang/ifThenElse x
                                   (\_1 -> p)
                                   (\_2 -> ohua.lang/recur p) in
                       c in
                   math/times10 g) in
               let y = a 95 in
               y
               |]
    -- (Let "a"
    --      (Lambda
    --           "i"
    --           (Let "g"
    --                (Let "p"
    --                     (("math/minus" `Apply` "i") `Apply` 10)
    --                     (Let "x"
    --                          (("math/lt" `Apply` "p") `Apply` 0)
    --                          (Let "c"
    --                               (Apply
    --                                    (Apply
    --                                         (Apply (sf IFuns.ifThenElse) "x")
    --                                         (Lambda "then" "p"))
    --                                    (Lambda "else" ((sf recur) `Apply` "p")))
    --                               "c")))
    --                ("math/times10" `Apply` "g")))
    --      (Let "y" ("a" `Apply` 95) "y"))

expectedHoferized :: Expression
expectedHoferized =
    [embedALang|
        let a_0 = \i ->
            let p = math/minus i 10 in
            let x = math/lt p 0 in
            let c = ohua.lang/ifThenElse x
                        (\_1 -> p)
                        (\_2 -> ohua.lang/recur p) in
            c in
        let a = ohua.lang/recur_hof a_0 in
        let y = a 95 in
        y
        |]
    -- Let "a_0"
    --     (Lambda
    --          "i"
    --          (Let "p"
    --               ("math/minus" `Apply` "i" `Apply` 10)
    --               (Let "x"
    --                    ("math/lt" `Apply` "p" `Apply` 0)
    --                    (Let "c"
    --                         (sf IFuns.ifThenElse `Apply` "x" `Apply`
    --                          Lambda "then" "p" `Apply`
    --                          Lambda "else" (sf recur `Apply` "p"))
    --                         "c"))))
    --     (Let "a" (sf recur_hof `Apply` "a_0") (Let "y" ("a" `Apply` 95) "y"))

expectedRewritten :: Expression
expectedRewritten =
    [embedALang|
                 let y =
                     let ctrls_0 = ohua.lang.marker/recur_start 95 in
                     let ctrl_0 = ohua.lang/nth 0 2 ctrls_0 in
                     let i = ohua.lang/nth 1 2 ctrls_0 in
                     let ctrl_1 = ohua.lang/ctrl ctrl_0 i in
                     let i_0 = ohua.lang/nth 0 1 ctrl_1 in
                     let p = math/minus i_0 10 in
                     let x = math/lt p 0 in let c = ohua.lang.marker/recur_end x p p in c in
                 y

               |]
    -- Let "y"
    --     (sf recur `Apply`
    --      (Lambda
    --           "e"
    --           (destructure
    --                "e"
    --                ["i"]
    --                (Let "p"
    --                     ("math/minus" `Apply` "i" `Apply` 10)
    --                     (Let "x"
    --                          ("math/lt" `Apply` "p" `Apply` 0)
    --                          (Let "c"
    --                               (sf IFuns.ifThenElse `Apply` "x" `Apply`
    --                                Lambda
    --                                    "_0"
    --                                    (Let "d"
    --                                         (sf IFuns.mkTuple `Apply`
    --                                          Lit (NumericLit 1) `Apply`
    --                                          (sf IFuns.id `Apply` "p") "d")) `Apply`
    --                                Lambda
    --                                    "_1"
    --                                    (Let "b"
    --                                         (sf IFuns.mkTuple `Apply`
    --                                          Lit (NumericLit 0) `Apply`
    --                                          (sf array `Apply` "p"))
    --                                         "b")))
    --                          "c")))) `Apply`
    --      (sf array `Apply` 95))
    --     "y"

-- TODO It would be sooo nice, if I could write my test expressions like this!
-- let y = ohua.lang/recur (\955 e ->
--                                 let (i) = e
--                                 in let p = math/- i $10
--                                    in let x = math/< p $0
--                                       in let c = ohua.lang/if x
--                                                              (\955 then ->
--                                                                        let d = ohua.lang/(,) (ohua.lang/false ()) (ohua.lang/id p)
--                                                                        in d)
--                                                              (\955 else ->
--                                                                        let b = ohua.lang/(,) (ohua.lang/true ()) (ohua.lang/array p)
--                                                                        in b)
--                                         in c)
--                         (ohua.lang/array $95)
-- in y
expectedLowered =
    [embedDFLang|
                let (m) = ohua.lang/id<1> (95) in
                let (ctrl_0, i, c) = dataflow ohua.lang/recurFun<2> (x, p, m, p) in
                let (ctrl_1) = dataflow ohua.lang/ctrl<5> (ctrl_0, i) in
                let (i_0) = ohua.lang/nth<6> (0, 1, ctrl_1) in
                let (p) = math/minus<7> (i_0, 10) in
                let (x) = math/lt<8> (p, 0) in
                c
                |]
    -- DFExpr
    --     [ LetExpr 1 "f" (EmbedSf "ohua.lang/array") [DFEnvVar 95] Nothing
    --     , LetExpr 3 ["i"] (EmbedSf "ohua.lang/id") [DFVar "e"] Nothing
    --     , LetExpr 4 "p" (EmbedSf "math/minus") [DFVar "i", DFEnvVar 10] Nothing
    --     , LetExpr 5 "x" (EmbedSf "math/lt") [DFVar "p", DFEnvVar 0] Nothing
    --     , LetExpr
    --           13
    --           ["then", "else"]
    --           (EmbedSf "ohua.lang/bool")
    --           [DFVar "x"]
    --           Nothing
    --     , LetExpr 14 ["p_0"] (DFFunction "ohua.lang/scope") [DFVar "p"] $
    --       Just "then"
    --     , LetExpr 7 "g" (EmbedSf "ohua.lang/id") [DFVar "p_0"] Nothing
    --     , LetExpr 8 "h" (EmbedSf "ohua.lang/false") [dfVarUnit] $ Just "then"
    --     , LetExpr 9 "d" (EmbedSf "ohua.lang/(,)") [DFVar "h", DFVar "g"] Nothing
    --     , LetExpr 15 ["p_1"] (DFFunction "ohua.lang/scope") [DFVar "p"] $
    --       Just "else"
    --     , LetExpr 10 "j" (EmbedSf "ohua.lang/array") [DFVar "p_1"] Nothing
    --     , LetExpr 11 "k" (EmbedSf "ohua.lang/true") [dfVarUnit] $ Just "else"
    --     , LetExpr
    --           12
    --           "b"
    --           (EmbedSf "ohua.lang/(,)")
    --           [DFVar "k", DFVar "j"]
    --           Nothing
    --     , LetExpr
    --           16
    --           "c"
    --           (DFFunction "ohua.lang/select")
    --           [DFVar "then", DFVar "d", DFVar "b"]
    --           Nothing
    --     , LetExpr
    --           17
    --           ["y", "e"]
    --           (DFFunction "ohua.lang/recur")
    --           [DFVar "f", DFVar "c"]
    --           Nothing
    --     ]
    --     "y"

-- Runs:
detect_recursion recExpr expectedExpr =
    findTailRecs True recExpr `shouldBe` expectedExpr

runPass pass expr = runSilentLoggingT $ runFromExpr def pass expr

noTailRec expr expected = do
    res <- (runPass (normalize >=> verifyTailRecursion) expr)
    res `shouldSatisfy` isLeft
    let Left msg = res
    toString msg `shouldStartWith` expected

rewritePass expr expected =
    runPass
        (findTailRecs True >=>
         normalize >=>
         verifyTailRecursion >=> rewriteAll)
        expr >>=
    ((`shouldBe` Right (showWithPretty expected)) . fmap showWithPretty)

lower expr expected =
    runPass
        (findTailRecs True >=>
         normalize >=>
         --verifyTailRecursion >=>
         rewriteAll >=>
         normalize >=>
         lowerALang >=>
         return . recurLowering . collapseNth (== recurStartMarker))
        expr >>=
    (\a -> fmap showWithPretty a `shouldBe` Right (showWithPretty expected))

spec :: Spec
spec
    -- describe "Phase 1: rec detection" $ do
    --     it "recursion with expression (let + id) on terminal branch" $
    --         detect_recursion
    --             recWithExprOnTerminalBranch
    --             expectedRecWithExprOnTerminalBranch
    --     it "recursion with var on terminal branch" $
    --         detect_recursion
    --             recWithVarOnlyOnTerminalBranch
    --             expectedRecWithVarOnlyOnTerminalBranch
    --     it "recursion with expression (let) on recursion branch" $
    --         detect_recursion
    --             recWithExprOnRecurBranch
    --             expectedRecWithExprOnRecurBranch
    --     it "recursion correctly structured" $
    --         detect_recursion
    --             recWithCallOnlyOnRecurBranch
    --             expectedRecWithCallOnlyOnRecurBranch
 = do
    describe "Verification:" $ do
        it "no tail recursion 1" $
            noTailRec notTailRecursive1
            "Recursion is not tail recursive! Last stmt: \"ohua.lang/ifThenElse x (\\955 _1 -> let d = ohua.lang/id p in d)\\n  (\\955 _2 -> let g = ohua.lang/recur p in let b = math/times10 g in b)\""
        it "no tail recursion 2" $
            noTailRec
                notTailRecursive2
                "Recursion is not tail recursive! Last stmt: \"math/times10 c\""
        it "no tail recursion 3" $
            noTailRec
                notTailRecursive3
                "Recursion is not tail recursive! Last stmt: \"math/times10 c\""
    describe "Phase 3: final ALang rewrite" $ do
        it "rewrites correct recursion" $
            rewritePass recWithCallOnlyOnRecurBranch expectedRewritten
    describe "Phase 4: DF lowering" $ do
        it "lowers correct recursion" $
            lower expectedRecWithCallOnlyOnRecurBranch expectedLowered
