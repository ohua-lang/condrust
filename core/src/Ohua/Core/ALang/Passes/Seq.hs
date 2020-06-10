{-|
Module      : $Header$
Description : Implementation for the transformation that ensures sequential side-effects.
Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:
It is the very same rewrite as for the true branch of the `ifRewrite`.
In fact, we could turn the `seq` into an if that is always true. We just would need a
new function `trueC :: a -> Bool` that always returns `True`.
Instead, we do a more performant version and introduce a `seqFun` operator that does this
and also creates the control signal (saving the `if` node).

Note that `seq` is about sequencing side-effects to I/O.
-}
module Ohua.Core.ALang.Passes.Seq where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Passes.Control
import qualified Ohua.Core.ALang.Refs as Refs (seq, seqFun)
import Ohua.Core.ALang.Util (lambdaArgsAndBody)

seqFunSf :: Expression
seqFunSf = Lit $ FunRefLit $ FunRef Refs.seqFun Nothing

-- | Assumes seq is applied as
--   seq(dep, \_ -> g)
--   This is to be enforced in the translation from the frontend language into ALang.
seqRewrite :: (Monad m, MonadGenBnd m) => Expression -> m Expression
seqRewrite (Let v a b) = Let v <$> seqRewrite a <*> seqRewrite b
seqRewrite (Lambda v e) = Lambda v <$> seqRewrite e
seqRewrite (Apply (Apply (Lit (FunRefLit (FunRef "ohua.lang/seq" Nothing))) dep) expr) = do
    expr' <- seqRewrite expr
    -- post traversal optimization
    ctrl <- generateBindingWith "ctrl"
    expr'' <- liftIntoCtrlCtxt ctrl expr'
    let expr''' = getExpr $ lambdaArgsAndBody expr''
    -- return $
    --     [ohualang|
    --       let $var:ctrl = Ohua.Core.lang/seqFun $var:dep in
    --         let result = $expr:expr' in
    --             result
    --                |]
    result <- generateBindingWith "result"
    return $ Let ctrl (Apply seqFunSf dep) $ Let result expr''' $ Var result
    where
        -- TODO check that this is actually () -> maybe it is better to check this
        --      via assuring that the expr was a lambda with a Unit argument.
        getExpr ((_:[]), e) = e
        getExpr (args, e) = error $ 
            "Expected () argument for expression to seq (" <> show e <> ") but got: " 
            <> show args 
            <> " This is a broken compiler invariant. Please report it."
seqRewrite e = return e
