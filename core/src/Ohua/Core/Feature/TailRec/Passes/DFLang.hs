{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Core.Feature.TailRec.Passes.DFLang where

import Ohua.Core.Prelude
import qualified Ohua.Types.Vector as V
import Data.Singletons

import Ohua.Core.DFLang.Lang
-- import Ohua.Core.DFLang.PPrint (prettyExprM)
import qualified Ohua.Core.Feature.TailRec.Passes.ALang as ALangPass
import Ohua.Core.DFLang.Passes (checkDefinedUsage)

import Data.List.NonEmpty as NE (toList)

-- | Here, we are actually tying the knot and create the final recurFun node (replacing recurStart)
--   the has the loop-back connection to the start of the recursion.
recurLowering :: forall m ty.MonadOhua m => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
recurLowering expr
 = checkDefinedUsage expr >> -- expresses a precondition for the below transformation
      do
        expr' <- transformExprTDM recurStartToRecurFun expr
        let expr'' = filterEnds expr'
        return expr''
  where
      recurStartToRecurFun :: NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
      recurStartToRecurFun (Let app@(PureDFFun (Destruct [_, _]) fun inp) rest)
        | fun == ALangPass.recurStartMarker = do
          recurFun <- findEnd (outsANew app) inp rest
          return $ Let recurFun rest
      recurStartToRecurFun (Let app rest) = Let app <$> recurStartToRecurFun rest
      recurStartToRecurFun e = return e

      filterEnds (Let app@PureDFFun{} cont) | fnDFApp app == ALangPass.recurEndMarker = cont
      filterEnds (Let app cont) = Let app $ filterEnds cont
      filterEnds v = v

      -- I have to play this game here because the type again is not precise enough!
      -- What this function actually wants to dispatch on, is a function that is not a state
      -- initializer function, i.e., it does not output anything that is later used as state.
      outsANew = map (Direct . DataBinding) . outsDFApp

      findEnd :: NonEmpty (OutData 'Data) -> NonEmpty (DFVar 'Data ty)
              -> NormalizedDFExpr ty -> m (DFApp 'BuiltIn ty)
      findEnd outs inp (Let app@PureDFFun{} cont) | fnDFApp app == ALangPass.recurEndMarker =
          let cond:(fixRef:recurArgs) = insDFApp app
              -- FIXME we don't need the var lists when we use the assertion
              -- that these two lists have the same size! and this is always
              -- true because these are the arguments to a call to the same
              -- function, i.e, the recursion!
              condIn = DFVar TypeVar $ DataBinding cond
              finalResultIn = DFVar TypeVar $ DataBinding fixRef
              recurInitArgs = NE.toList inp
              recurArgs' = map (DFVar TypeVar . DataBinding) recurArgs

              ctrlOut = head outs
              recurArgsOuts = tail outs
              -- assumption: the end marker only has the final output
              finalResultOut = last $ outsANew app

              fun :: [(OutData 'Data, DFVar 'Data ty, DFVar 'Data ty)] -> V.SNat n
                  -> DFApp 'BuiltIn ty
              fun xs snat =
                let vec = V.fromList snat xs
                    recurArgsOuts' = V.map (\(x,_,_) -> x) vec
                    recurInitArgs' = V.map (\(_,x,_) -> x) vec
                    recurArgs'' = V.map (\(_,_,x) -> x) vec
                in RecurFun
                    finalResultOut
                    ctrlOut
                    recurArgsOuts'
                    recurInitArgs'
                    recurArgs''
                    condIn
                    finalResultIn
              toRecurFun =
                let xs = zip3 recurArgsOuts recurInitArgs recurArgs'
                in withSomeSing (V.nlength xs) (fun xs)

              rf = assert
                    (length recurArgsOuts == length recurInitArgs
                     && length recurInitArgs == length recurArgs')
                    toRecurFun

          in pure rf
      findEnd outs inp (Let _ cont) = findEnd outs inp cont
      -- FIXME This looks like we should perform this transformation differently, so we
      -- can avoid this failure case. 
      findEnd _ _ (Var v) = failWith $ "Did not find end marker for recursion. Hit var: " <> show v
