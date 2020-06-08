module Ohua.Frontend.Transform.Calls where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Data.HashSet as HS 
import Data.Functor.Foldable (cata, embed)
import Control.Lens.Combinators (over)
import Control.Lens.Plated (plate)


--      The implementation is quite straight forward: collect the variables and check for
--      references that are not defined as variables (usually in function position).
--      Since we assume that the program passed the Go compiler, these must be references
--      to functions in the current namespace.
-- TODO: Maybe this should check against a set of Namespace bindings?!
--       What would it be if it wasn't in this set? Invariant?
makeImplicitFunctionBindingsExplicit :: Expr -> Expr
makeImplicitFunctionBindingsExplicit = go HS.empty
    where
        -- see example 5 in
        -- https://ndmitchell.com/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf
        go scope (LetE v a b) = 
            let scope' = HS.union scope $ HS.fromList $ goPat v 
            in LetE v (go scope' a) (go scope' b)
        go scope (LamE vs b) =
            let scope' = HS.union scope $ HS.fromList $ join $ Ohua.Prelude.map goPat vs 
            in LamE vs (go scope' b)
        go scope (VarE bdg) | not (HS.member bdg scope) = 
                LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) bdg) Nothing 
        go scope e = descend (go scope) e 
        descend = over plate -- note composOp = descend = over plate -> https://www.stackage.org/haddock/lts-14.25/lens-4.17.1/Control-Lens-Plated.html#v:para (below)
        goPat UnitP = []
        goPat (VarP bdg) = [bdg]
        goPat (TupP ps) = join $ Ohua.Prelude.map goPat ps
