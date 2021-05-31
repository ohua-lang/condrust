{-|
Module      : $Header$
Description : Implementation of ALang transformation for If.
Copyright   : (c) Sebastian Ertel, Justus Adam 2018. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:
As described in Ohua.Core.ALang.Passes.Control, the resulting expression will look as follows:

@
let dataGen = ... in
 let (data :: Control a, ctrlVar :: Control (Maybe Int), size :: Int) = smapFun dataGen in
  let (a,b,c) = ctrl ctrlVar a b c in
   let result = body in -- lifted into control context
    let resultData = collect size result in
      resultData
@

Note that `smapFun` must be an operator because the first two outputs reside in the control
context while the last one doesn't.
-}
module Ohua.Core.ALang.Passes.Smap where

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Passes.Control
import qualified Ohua.Core.ALang.Refs as Refs
import Ohua.Core.ALang.Util (lambdaArgsAndBody, mkDestructured, renameVar)
import Ohua.Core.Prelude

import Ohua.Core.ALang.PPrint ()

import Data.List.NonEmpty (fromList)

smapSfFun :: Expr ty
smapSfFun = Lit $ FunRefLit $ FunRef Refs.smapFun Nothing $ FunType $ Right $ fromList [TypeVar]

collectSf :: Expr ty
collectSf = Lit $ FunRefLit $ FunRef Refs.collect Nothing $ FunType $ Right $ fromList [TypeVar, TypeVar]

smapRewrite :: (Monad m, MonadGenBnd m) => Expr ty -> m (Expr ty)
smapRewrite =
    rewriteM $ \case
        PureFunction op _ `Apply` lamExpr `Apply` dataGen
            | op == Refs.smap -> Just <$> do
                lamExpr' <- smapRewrite lamExpr
    -- post traversal optimization
                ctrlVar <- generateBindingWith "ctrl"
                lamExpr'' <- liftIntoCtrlCtxt ctrlVar lamExpr'
                let ([inBnd], expr) = case lambdaArgsAndBody lamExpr'' of 
                                        e@([_], _) -> e
                                        e -> error $ "Pattern match failure. Got pattern: " <> quickRender e 
                d <- generateBindingWith "d"
                let expr' = renameVar expr (Var inBnd, d)
  --   [ohualang|
  --     let (d, $var:ctrlVar, size) = Ohua.Core.lang/smapFun $var:dataGen in
  --      let (a,b,c) = ctrl $var:ctrlVar a b c in
  --       let result = $expr:body' in -- lifted into control context
  --        let resultList = collect size result in
  --          resultList
  -- (this breaks haddock) |]
                size <- generateBindingWith "size"
                ctrls <- generateBindingWith "ctrls"
                result <- generateBindingWith "result"
                resultList <- generateBindingWith "resultList"
                return $
                    Let ctrls (Apply smapSfFun dataGen) $
                    mkDestructured [d, ctrlVar, size] ctrls $
                    Let result expr' $
                    Let
                        resultList
                        (Apply (Apply collectSf $ Var size) $ Var result) $
                    Var resultList
        _ -> pure Nothing
