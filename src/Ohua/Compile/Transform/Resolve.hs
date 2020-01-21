{-|

Module      : $Header$
Description : Transformation for splicing in algos for algo references.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This module is a transformation that turns a function reference into an expression.
The resulting expression does not contain any (function reference) anymore.
Function references in this transformation are just references to variable that refer to
a function. That is, this transformation does not deal with namespaces or such.

-}

module Ohua.Compile.Transform.Resolve where

import Data.HashMap.Strict as HM

-- FIXME where is this needed? how can it be possible that the frontend expression language has these namespaces?
stdNamespace :: (NSRef, [Binding])
stdNamespace =
    ( ["ohua", "lang"]
    , ["id", "smap", "if"] -- TODO complete list
     )

primitives :: HM.HashMap Binding Expression
primitives = HM.fromList []

-- TODO: It feels to me like this function needs a rewrite. I'm quite sure there was no testing at
--       all to make sure it works for algorithms from other namespaces.
--       More urgently, we need to define what a namespace reference that a parser returns should look like.
--       For example in Go:
--       import some/other/ns
--                ^
--                |
--                 - What is this?
--       ns.foo()
--        ^
--        |
--         - Or this?
--
--       Or in Rust:
--       use some::other::ns{foo};
--      
--       foo(); <-- This is not a local algo!
--
-- For this transformation, the expression only has the following form:
-- > some/other/ns:foo()
--   |               |
--    ---------------
--           |
--            -> This is a function literal with a qualified binding.
--
-- The algo is registered with the key 
-- > some/other/ns:foo
-- In the form of a qualified binding.
resolveNS ::
       forall m. (MonadError Error m)
    => (P.Namespace, HashMap QualifiedBinding Expression)
    -> m P.Namespace
resolveNS (ns, registry) = return $ over algos (map (over algoCode resolveExpr)) ns
    where 
        resolveExpr :: Expr -> Expr
        resolveExpr = cata $ \exp ->
            case exp of
                LitEF (FunRefLit (FunRef ref _id) ) | HM.member ref registry ->
                    HM.get ref registry
                e -> embed e

