{-# LANGUAGE PartialTypeSignatures #-}

module Ohua.Core.Feature.TailRec.Passes.DFLang where

import Ohua.Core.Prelude

import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs as Refs
import qualified Ohua.Core.Feature.TailRec.Passes.ALang as ALangPass
import Ohua.Core.DFLang.Passes (checkDefinedUsage)

import Data.Sequence as DS (fromList)

-- | Here, we are actually tying the knot and create the final recurFun node (replacing recurStart)
--   the has the loop-back connection to the start of the recursion.
recurLowering :: (MonadOhua m) => NormalizedDFExpr -> m NormalizedDFExpr
recurLowering expr
  -- 1. Find the recurFun with two outputs
 = checkDefinedUsage expr >> -- expresses a precondition for the below transformation
    transformM f expr
  where
    f l@(LetPureFun (PureFun (Destruct [out1, out2]) fun inp) rest) 
      | fun == ALangPass.recurStartMarker =
        let (endFunction, rest') = findEnd rest
            fixRef:cond:recurArgs = insDFApp endFunction
        in LetPureDFFun 
            (PureDFFun 
              (Destruct $ [out1, out2] <> outsDFApp endFunction) 
              Refs.recurFun 
                -- FIXME we don't need the var lists when we use the assertion
                -- that these two lists have the same size! and this is always
                -- true because these are the arguments to a call to the same
                -- function, i.e, the recursion!
              $ fixRef : cond :| inp <> recurArgs)
            rest'
    f e = e

    findEnd (LetPureDFFun app cont) | fnDFApp app == ALangPass.recurEndMarker = return (app, cont)
    findEnd (LetPureDFFun app cont) = second (LetPureDFFun app) <$> findEnd cont
    findEnd (LetStateDFFun app cont) = second (LetStateDFFun app) <$> findEnd cont
    -- FIXME This looks like we should perform this transformation differently, so we
    -- can avoid this failure case. 
    findEnd (VarDFFun _) = failWith "Did not find end marker for recursion"