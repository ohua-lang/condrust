module Ohua.Backend.CodeGen where

import Universum

import qualified Ohua.DFGraph as OC
import qualified One4All.Lang as O4A

genCode :: OC.OutGraph -> O4A.Expr
genCode graph = do
    arcCode <- genArcs graph
    arcCode

-- arc generation
genArcs :: OC.OutGraph -> O4A.Expr
genArcs (OC.OutGraph operators arcs _) =
    let localArcs = filter isNotEnvArc arcs
     in do outArcs <- [genOutArcVariable x operators | x <- localArcs]
           inArcs <- [genInArcVariable x | x <- localArcs]
           undefined
  -- continue here! generierung der arcs schreiben (ich glaube $x ist für variableneinsetzung)

isNotEnvArc :: OC.Arc envExpr -> Bool
isNotEnvArc (OC.Arc _ (OC.LocalSource _)) = True
isNotEnvArc (OC.Arc _ (OC.EnvSource _)) = False

genOutArcVariable :: OC.Arc envExpr -> [OC.Operator] -> O4A.Var
genOutArcVariable = undefined

genInArcVariable :: OC.Arc envExpr -> O4A.Var
genInArcVariable (OC.Arc (OC.Target op (-1)) _) =
    O4A.V ("sf_" ++ show op ++ "_in_ctrl" :: String)
genInArcVariable (OC.Arc (OC.Target op idx) _)
    | idx > -2 = O4A.V ("sf_" ++ show op ++ "_in_" ++ show idx :: String)

-- utilities
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
