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
-- import Ohua.Core.ALang.Passes.Control
import qualified Ohua.Core.ALang.Refs as Refs (seqFun)
-- import Ohua.Core.ALang.Util (lambdaArgsAndBody)

seqFunSf :: Expr ty
-- Question: Whats the type supposed to be?
seqFunSf = Lit $ FunRefLit $ FunRef Refs.seqFun Nothing $ FunType [TypeVar] TypeVar

-- The below version of seq dates back to a time where did not have the concept of
-- state threads. State threads sequence side-effects now. And the only means for
-- seq is essentially when an algorithm wishes to return the result of computation without
-- any data dependencies. As such, all that seq is doing is creating this fake dependency
-- to preserve the semantics. As of now, it is not even clear whether that is something
-- desirable because it automatically prevents parallelism.
-- Therefore, I'm deprecating the old algorithm for now but leave it in until we had time
-- to think about this harder or came across a use case that would varant this.
-- Note that `seq` semantically always was a `const`, i.e., `seq :: a -> b`.

--
-- -- | Assumes seq is applied as
-- --   seq(dep, \_ -> g)
-- --   This is to be enforced in the translation from the frontend language into ALang.
-- seqRewrite :: (Monad m, MonadGenBnd m) => Expr ty -> m (Expr ty)
-- seqRewrite (Let v a b) = Let v <$> seqRewrite a <*> seqRewrite b
-- seqRewrite (Lambda v e) = Lambda v <$> seqRewrite e
-- seqRewrite (Apply (Apply (Lit (FunRefLit (FunRef "ohua.lang/seq" Nothing _))) dep) expr) = do
--     expr' <- seqRewrite expr
--     -- post traversal optimization
--     ctrl <- generateBindingWith "ctrl"
--     expr'' <- liftIntoCtrlCtxt ctrl expr'
--     let expr''' = getExpr $ lambdaArgsAndBody expr''
--     -- return $
--     --     [ohualang|
--     --       let $var:ctrl = Ohua.Core.lang/seqFun $var:dep in
--     --         let result = $expr:expr' in
--     --             result
--     --                |]
--     result <- generateBindingWith "result"
--     return $ Let ctrl (Apply seqFunSf dep) $ Let result expr''' $ Var result
--     where
--         -- TODO check that this is actually () -> maybe it is better to check this
--         --      via assuring that the expr was a lambda with a Unit argument.
--         getExpr ([_], e) = e
--         getExpr (args, e) = error $ 
--             "Expected () argument for expression to seq (" <> show e <> ") but got: " 
--             <> show args 
--             <> " This is a broken compiler invariant. Please report it."
-- seqRewrite e = return e

seqRewrite :: (Monad m) => Expr ty -> m (Expr ty)
seqRewrite = pure
