{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Core.Feature.TailRec.Passes.DFLang where

import Ohua.Core.Prelude

import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs as Refs
import qualified Ohua.Core.Feature.TailRec.Passes.ALang as ALangPass
import Ohua.Core.DFLang.Passes (checkDefinedUsage)

import Data.List.NonEmpty ((<|))

-- | Here, we are actually tying the knot and create the final recurFun node (replacing recurStart)
--   the has the loop-back connection to the start of the recursion.
recurLowering :: MonadOhua m => NormalizedDFExpr -> m NormalizedDFExpr
recurLowering expr
  -- 1. Find the recurFun with two outputs
 = checkDefinedUsage expr >> -- expresses a precondition for the below transformation
      transformExprM f expr
  where
      f :: MonadOhua m => NormalizedDFExpr -> m NormalizedDFExpr
      f (Let app@(PureDFFun (Destruct [_, _]) fun inp) rest) 
        | fun == ALangPass.recurStartMarker = findEnd (outsANew app) inp rest
      f (Let _ rest) = f rest
      f e = return e

      -- I have to play this game here because the type again is not precise enough!
      -- What this function actually wants to dispatch on, is a function that is not a state 
      -- initializer function, i.e., it does not output anything that is later used as state.
      outsANew = map (Direct . DataBinding) . outsDFApp

      findEnd :: MonadOhua m => NonEmpty (OutData 'Data) -> NonEmpty DFVar -> NormalizedDFExpr -> m NormalizedDFExpr
      findEnd outs inp (Let app cont) | fnDFApp app == ALangPass.recurEndMarker = 
          let fixRef:(cond:recurArgs) = insDFApp app
                -- FIXME we don't need the var lists when we use the assertion
                -- that these two lists have the same size! and this is always
                -- true because these are the arguments to a call to the same
                -- function, i.e, the recursion!
              inp' = (DFVar $ DataBinding fixRef) <| [DFVar $ DataBinding cond] <> inp 
              inp'' = maybe inp' (inp' <>) $ nonEmpty $ map (DFVar . DataBinding) recurArgs
              outs'' = Destruct (outs <> map (Direct . DataBinding) (outsDFApp app))
          in pure $
              Let (PureDFFun outs'' Refs.recurFun inp'') cont
      findEnd outs inp (Let app cont) = Let app <$> findEnd outs inp cont
      -- FIXME This looks like we should perform this transformation differently, so we
      -- can avoid this failure case. 
      findEnd _ _ (Var _) = failWith "Did not find end marker for recursion"