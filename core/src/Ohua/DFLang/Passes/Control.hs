module Ohua.DFLang.Passes.Control where

import Ohua.Prelude

import qualified Data.List.NonEmpty as NE (fromList)
import Data.Sequence as DS ((><), fromList)

import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util (findAllExprs, findUsages, removeAllExprs)

-- | The `ctrl` on the ALang level is a function. The `ctrl` on the DFLang level
--   is an operator and as such we can remove the destructuring and do that
--   directly in the operator.
optimizeCtrl :: DFExpr -> DFExpr
optimizeCtrl (DFExpr letExprs returnVar) =
    let ctrls = toList $ findAllExprs Refs.ctrl letExprs
        (newCtrls, orphanedNths) = unzip $ toList $ map updateCtrl ctrls
        orphanedNths' = catMaybes $ join orphanedNths
        letExprs' =
            removeAllExprs (DS.fromList $ ctrls ++ orphanedNths') letExprs
     in flip DFExpr returnVar $ letExprs' >< DS.fromList newCtrls
  where
    updateCtrl ctrl@(LetExpr {output = outs}) =
        let (newOuts, nths) =
                Ohua.Prelude.unzip $
                flip map outs $ \out ->
                    let (nth:[]) = findUsages out letExprs
                        nthOuts = output nth
                     in if (length nthOuts) == 1
                            then (head $ NE.fromList nthOuts, Just nth)
                            else (out, Nothing)
         in (ctrl {output = newOuts}, nths)
