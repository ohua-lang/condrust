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
We perform lambda lifting on both branches (after normalization):

@
let result = if cond
                (\() -> f a b)
                (\() -> g c d)
@

into:

@
let result = if cond
                (\() -> let x = scope a b
                         in let x0 = nth 0 x
                             in let x1 = nth 1 x
                                 in f x0 x1
                (\() -> let x = scope c d
                         in let x0 = nth 0 x
                             in let x1 = nth 1 x
                                 in g x0 x1
@

The translation of `if` itself then produces the following code:

@
let cond :: Bool = ...
  let ncond :: Bool = not cond in
    let tBranchCtrl :: Control Bool = ctrl cond in
      let fBranchCtrl :: Control Bool = ctrl ncond in
        let resultTrue :: Control a = (\() -> ... true branch expression ... ) tBranchCtrl in
          let resultFalse :: Control a = (\() -> ... false branch expression ... ) fBranchCtrl in
            let result :: a = select tBranchCtrl fBranchCtrl resultTrue resultFalse in
              result
@

Currently, we can perform this transformation without type annotiations because of the following reasons:

  1. The `Bool` type of `cond` is verify by the `not` function which requires a `Bool`.
  2. The `Control` types can be easily found via checking the source of the local, i.e., the binding that created the local.

Semantically, each branch runs in a `Control` context and as such that resulting value depends on
control value applied to this computation. The `select` then retrieves the final result using both control signals.

We finally simplify the `select` known that

  1. The `ctrl` calls will be removed by the lowering pass because its inputs are already of type `Bool`.
  2. The `fBranchCtrl` value is the negation of the `tBranchCtrl` value.

As such, we write:

@
let result :: a = select cond resultTrue resultFalse in ...
@

Note that we translate the applications `(\() -> ... branch ...) ctrlVal` into the following:

@
let x = scope ctrl a b in
 ...
  let y = idependentFn ctrl in
   ...
@

In fact, this applies the control value to the lambda expression.
As a result, we can write the following:

@
let cond :: Bool = ...
  let ncond :: Bool = not cond in
    let tBranchCtrl :: Control Bool = ctrl cond in
      let fBranchCtrl :: Control Bool = ctrl ncond in
        let resultTrue :: Control a = ... true branch expression ... in
          let resultFalse :: Control a = ... false branch expression ... in
            let result :: a = select tBranchCtrl fBranchCtrl resultTrue resultFalse in
              result
@

Now this expression can be lowered to DFLang without any further ado.
The lowering itself should be sensitive to value of type `Control` and perform the
respective steps.
As a last step, we can optimize the DFLang expression to end up with:

@
let cond :: Bool = ...
  let ncond :: Bool = not cond in
    let resultTrue :: Control a = (\() -> ... true branch expression ... ) cond in
      let resultFalse :: Control a = (\() -> ... false branch expression ... ) ncond in
        let result :: a = select cond resultTrue resultFalse in
          result
@
-}
{-# LANGUAGE CPP #-}
module Ohua.Core.ALang.Passes.If where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Passes.Control (liftIntoCtrlCtxt)
import qualified Ohua.Core.InternalFunctions as IFuns (ifFun, ifThenElse, select)
import Ohua.Core.ALang.Util (mkDestructured)

-- import Control.Category ((>>>))
import qualified Data.Text as T

selectSf :: VarType ty -> Expr ty
selectSf vty = Lit $ FunRefLit $ FunRef IFuns.select Nothing $ FunType [TypeBool, vty, vty] vty

-- | The controle node that triggers either the then or the else branch, depending on the input bool
ifFunSf :: Expr ty
ifFunSf = Lit $ FunRefLit $ FunRef IFuns.ifFun Nothing $ FunType [TypeBool] (TupleTy $ controlSignalType:|[controlSignalType])

-- | Just a literal representing an if-function i.e. \con t1 t2 : if cond then t1 else t2
ifSf :: VarType ty -> Expr ty
ifSf vty = Lit $ FunRefLit $ FunRef IFuns.ifThenElse Nothing $ FunType [TypeBool, vty, vty] vty


-- This is a proposal for `ifRewrite` that uses plated to make sure the
-- recursion is handled correctly. As far as I can tell the other version does
-- not recurse properly onto the branches.
-- (Sebastian) The recursion is handled properly below and this version is incorrect.
--             However scrapping the boilerplate is definitely a good thing. It should be
--             a transformM though.
ifRewrite :: (Monad m, MonadGenBnd m, MonadError Error m) => Expr ty -> m (Expr ty)
ifRewrite = transformM $ \case
    Lit (FunRefLit (FunRef f _ _)) `Apply` cond `Apply` trueBranch `Apply` falseBranch
        | f == IFuns.ifThenElse -> do
            case (trueBranch,falseBranch) of
              (Lambda trueIn trueBody, Lambda falseIn falseBody) | isUnit trueIn && isUnit falseIn -> do
                ctrlTrueBnd <- generateBindingWith "ctrlTrue"
                ctrlFalseBnd <- generateBindingWith "ctrlFalse"
                ctrlsBnd <- generateBindingWith "ctrls"
                trueResultBnd <- generateBindingWith "trueResult"
                falseResultBnd <- generateBindingWith "falseResult"
                resultBnd <- generateBindingWith "result"
                let (typeTrue, typeFalse) = (exprType trueBody, exprType falseBody)
                assertE (typeTrue == typeFalse) $
                        " Error: Types of branching experssion do not match. Cannot decide the type of" <> 
                        " controle nodes collecting from both branches"
                let ctrlTrue = (TBind ctrlTrueBnd controlSignalType)
                    ctrlFalse= (TBind ctrlFalseBnd controlSignalType)
                    ctrls = (TBind ctrlsBnd (TupleTy $ controlSignalType:|[controlSignalType]))
                    trueResult = (TBind trueResultBnd typeTrue)
                    falseResult = (TBind falseResultBnd typeFalse)
                    -- We can take either true or false type for the result
                    result = (TBind resultBnd typeTrue)
                    
                trueBranch' <- liftIntoCtrlCtxt ctrlTrue trueBody
                falseBranch' <- liftIntoCtrlCtxt ctrlFalse falseBody   

                return $
               
                    Let ctrls (Apply ifFunSf cond) $
                    mkDestructured [ctrlTrue, ctrlFalse] ctrls $
                    Let trueResult trueBranch' $
                    Let falseResult falseBranch' $
                    Let
                    result
                    (Apply (Apply (Apply (selectSf typeTrue) cond) $ Var trueResult) $
                     Var falseResult) $
                    Var result
              _ -> throwError $ "Found if with unexpected, non-unit-lambda branch(es)\ntrue:\n " <> show trueBranch <> "\nfalse:\n" <> show falseBranch
      where
        isUnit (TBind bnd ty) = ty == TypeUnit
    e -> pure e
