module Ohua.Backend.CodeGen where

import Universum

import qualified Ohua.DFGraph as OC
import qualified Ohua.Types as OT
import qualified One4All.Lang as O4A

genCode :: OC.OutGraph -> O4A.Expr
genCode graph = do
    arcCode <- genArcs graph
    arcCode

-- arc generation
genArcs :: OC.OutGraph -> O4A.Expr
genArcs (OC.OutGraph operators arcs _) =
    let localArcs = filter isNotEnvArc arcs
     -- FIXME Why is this supposed to be a monad?
     in do outArcs <- [genOutArcVariable x | x <- localArcs]
           inArcs <- [genInArcVariable x | x <- localArcs]
           undefined
  -- TODO: continue here! write arc generation

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

-- utilities
getOpId :: OC.Source envExpr -> OT.FnId
getOpId (OC.LocalSource (OC.Target id _)) = id
getOpId (OC.EnvSource id) = undefined

getOutIdxFromSrc :: OC.Source envExpr -> Int
getOutIdxFromSrc (OC.LocalSource (OC.Target _ idx)) = idx
getOutIdxFromSrc (OC.EnvSource idx) = undefined

isFinalBinding :: O4A.Var -> Bool
isFinalBinding (O4A.V "toBeCompleted") = True
isFinalBinding _ = False

concatExpr :: O4A.Expr -> O4A.Expr -> O4A.Expr
concatExpr tree expr =
    case tree of
        O4A.Binding var
            | isFinalBinding var -> expr
            | otherwise -> O4A.Binding var
        O4A.Apply ex1 ex2 ->
            O4A.Apply (concatExpr ex1 expr) (concatExpr ex2 expr)
        O4A.Lambda v ex -> O4A.Lambda v (concatExpr ex expr)
        O4A.Let v ex1 ex2 ->
            O4A.Let v (concatExpr ex1 expr) (concatExpr ex2 expr)
        tree -> tree
