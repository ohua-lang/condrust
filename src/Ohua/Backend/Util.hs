module Ohua.Backend.Util where

import Universum

import qualified Ohua.DFGraph as OC
import qualified Ohua.Types as OT
import qualified One4All.Lang as O4A

getInArcs :: OT.FnId -> [OC.Arc envArc] -> [OC.Arc envArc]
getInArcs op arcs = filter (\(OC.Arc (OC.Target id _) _) -> id == op) arcs

getOutArcs :: OT.FnId -> [OC.Arc envArc] -> [OC.Arc envArc]
getOutArcs op arcs =
    filter
        (\(OC.Arc _ src) ->
             case src of
                 (OC.LocalSource (OC.Target id _)) -> id == op
                 (OC.EnvSource _) -> False)
        arcs

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
