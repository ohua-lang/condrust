{-# LANGUAGE OverloadedLists #-}
-- |
-- Module      : $Header$
-- Description : Tests for lowering the frontend language into ALang.
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module LoweringSpec
    ( spec
    )  where

import Ohua.Prelude

import Test.Hspec

import Ohua.Frontend.Lang
import Ohua.Compile.Lower.FrLang
import Ohua.Core.ALang.Lang
import Ohua.Core.Types 
import qualified Ohua.Core.ALang.Refs as ALangRefs
-- import Ohua.Core.ALang.PPrint (Pretty(pretty))
import qualified Data.HashSet as HS
import Data.List.NonEmpty (fromList)

spec :: Spec
spec =
    describe "removing destructuring" $ do
        let mkNth0 objBnd i total =
                pureFunction ALangRefs.nth Nothing (FunType $ Right $ fromList [TypeVar,TypeVar,TypeVar]) `Apply` Lit (NumericLit i) `Apply`
                Lit (NumericLit total) `Apply`
                Var objBnd
            runRemDestr =
                runCompM LevelDebug .
                toAlang' (HS.fromList ["a", "b", "c"])
        it "removes destructuring from lets" $
            let objBnd = TBind "d" (TupleTy $ TypeVar :| [TypeVar, TypeVar])
                mkNth = mkNth0 objBnd
                result = Let
                    objBnd
                    (Var (TBind "x" TypeVar))
                    (Let (TBind "a" TypeVar) (mkNth 0 3) $
                     Let (TBind "b" TypeVar) (mkNth 1 3) $ 
                     Let (TBind "c" TypeVar) (mkNth 2 3) 
                     (Var $ TBind "y" TypeVar)) 
             in runRemDestr (LetE ["a", "b", "c"] "x" "y") -- [embedALang| let (a, b, c) = x in y |]
                `shouldReturn`
                result
                
        it "removes destructuring from lambdas" $
            let objBnd = TBind "d" (TupleTy $ TypeVar :| [TypeVar, TypeVar])
                mkNth = mkNth0 objBnd
             in runRemDestr (LamE [["a", "b", "c"]] "y") -- [embedALang| \(a, b, c) -> y |]
                `shouldReturn`
                Lambda
                    objBnd
                    (Let (TBind "a" TypeVar) (mkNth 0 3) $
                     Let (TBind "b" TypeVar) (mkNth 1 3) $ 
                     Let (TBind "c" TypeVar) (mkNth 2 3) 
                     (Var $ TBind "y" TypeVar))
