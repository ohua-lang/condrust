{-# LANGUAGE LambdaCase #-}
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
import qualified Ohua.Core.InternalFunctions  as IFuns
import Ohua.Core.ALang.Util (lambdaArgsAndBody, mkDestructured, renameVar)
import Ohua.Core.Prelude

import Ohua.Core.ALang.PPrint ()


smapSfFun :: OhuaType ty Resolved -> OhuaType ty Resolved -> Expr embExpr ty
-- Takes a collection and returns (contained Dt, control signal, Nat)
-- I can get the data type from the input type of the function
smapSfFun collTy elemTy = Lit $ FunRefLit $ FunRef IFuns.smapFun $ FunType (Right $ collTy :| []) (TType (elemTy :| [controlSignalType, IType TypeNat]))

collectSf :: OhuaType ty Resolved -> Expr embExpr ty
-- Takes a nat and the return type t of the function and returns [t]
collectSf outTy = Lit $ FunRefLit $ FunRef IFuns.collect $ FunType (Right $ IType TypeNat :| [outTy]) (IType $ TypeList outTy)

smapRewrite :: (Monad m, MonadGenBnd m, Show embExpr) => Expr embExpr ty -> m (Expr embExpr ty)
smapRewrite =
    rewriteM $ \case
        PureFunctionTy fnName fnTy `Apply` lamExpr `Apply` dataGen
            | fnName == IFuns.smap -> Just <$> do
                lamExpr' <- smapRewrite lamExpr
    -- post traversal optimization
                ctrlVarBnd <- generateBindingWith "ctrl"
                let ctrlVar = TBind ctrlVarBnd controlSignalType
                let innerFunRet = exprType lamExpr
                    -- ToDo: get input type from lambda term
                    innerFunInput = case funType lamExpr of
                        Just (FunType (Right (inTy :| [] )) _out) -> inTy
                        Just (FunType (Right (it   :| its)) _out) -> TType (it:|its)
                        _ -> error $ "I expected a function with at least one input in an smapRewrite, but got: "
                                            <> quickRender lamExpr
                                            <> ".Please report this error."
                    collectionType = exprType dataGen
                lamExpr'' <- liftIntoCtrlCtxt ctrlVar lamExpr'
                let ([inSt@(TBind _inBnd sTy)], expr) = case lambdaArgsAndBody lamExpr'' of
                                        e@([_], _) -> e
                                        e -> error $ "Pattern match failure. Got pattern: " <> quickRender e
                dBnd <- generateBindingWith "d"
                let d = TBind dBnd sTy
                    expr' = renameVar expr (Var inSt, d)
  --   [ohualang|
  --     let (d, $var:ctrlVar, size) = Ohua.Core.lang/smapFun $var:dataGen in
  --      let (a,b,c) = ctrl $var:ctrlVar a b c in
  --       let result = $expr:body' in -- lifted into control context
  --        let resultList = collect size result in
  --          resultList
  -- (this breaks haddock) |]
                sizeB <- generateBindingWith "size"
                let size = TBind sizeB (IType TypeNat)
                ctrlsB <- generateBindingWith "ctrls"
                let ctrls = TBind ctrlsB (TType $ controlSignalType:|[controlSignalType])
                resultB <- generateBindingWith "result"
                let result = TBind resultB innerFunRet
                resultListB <- generateBindingWith "resultList"
                let resultList = TBind resultListB (IType $ TypeList (IType TypeUnit))

                return $
                    Let ctrls (Apply (smapSfFun collectionType innerFunInput) dataGen) $
                    mkDestructured [d, ctrlVar, size] ctrls $
                    Let result expr' $
                    Let
                        resultList
                        (Apply (Apply (collectSf innerFunRet) $ Var size) $ Var result) $
                    Var resultList
        _ -> pure Nothing
