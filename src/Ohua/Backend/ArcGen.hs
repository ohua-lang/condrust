{-# LANGUAGE QuasiQuotes #-}

module Ohua.Backend.ArcGen where

import Universum

import qualified Ohua.DFGraph as OC
import qualified Ohua.Types as OT
import qualified One4All.Lang as O4A
import One4All.Quote

import Ohua.Backend.Util

createArcs :: [(O4A.Var, O4A.Var)] -> O4A.Expr
createArcs ((inp, out):xs) =
    [o4a|
        let chan = channel () in
        let $var:out = nth 0 chan in
        let $var:inp = nth 1 chan in
         tree
    |]
  where
    tree = createArcs xs
createArcs [] = O4A.Binding (O4A.V "toBeCompleted")

genArcs :: OC.OutGraph -> O4A.Expr
genArcs (OC.OutGraph operators arcs _) =
    let localArcs = filter isNotEnvArc arcs
     -- FIXME Why is this supposed to be a monad?
     in do let outArcs = [genOutArcVariable x | x <- localArcs]
           let inArcs = [genInArcVariable x | x <- localArcs]
           createArcs $ zip inArcs outArcs

isNotEnvArc :: OC.Arc envExpr -> Bool
isNotEnvArc (OC.Arc _ (OC.LocalSource _)) = True
isNotEnvArc (OC.Arc _ (OC.EnvSource _)) = False

genOutArcVariable :: OC.Arc envExpr -> O4A.Var
genOutArcVariable (OC.Arc target src) =
    let out_port = genOutArcVariable' (getOpId src) (getOutIdxFromSrc src)
     in let in_port = genInArcVariable (OC.Arc target src)
         in case in_port of
                O4A.V s -> O4A.V (show out_port ++ "__" ++ s)
                O4A.AV _ -> undefined

-- TODO: This originally used the respective operator to check whether the arc originated from a SFN or a dataflow operator. However, this destinction does not (yet) exist in the DFGraph data structure and hence can not be used to verify correctness.
genOutArcVariable' :: OT.FnId -> Int -> String
genOutArcVariable' op idx =
    let computed_idx =
            case idx of
                idx
                    | idx == (-1) -> 0
                idx
                    | idx >= 0 -> idx
                otherwise -> undefined -- error "Invalid operator index: " ++ show idx
     in "sf_" ++ show op ++ "_out_" ++ show computed_idx

genInArcVariable :: OC.Arc envExpr -> O4A.Var
genInArcVariable (OC.Arc (OC.Target op (-1)) _) =
    O4A.V ("sf_" ++ show op ++ "_in_ctrl" :: String)
genInArcVariable (OC.Arc (OC.Target op idx) _)
    | idx > -2 = O4A.V ("sf_" ++ show op ++ "_in_" ++ show idx :: String)
