{-# LANGUAGE QuasiQuotes #-}

module Ohua.Backend.TaskGen where

import Universum

import qualified Ohua.DFGraph as OC
import qualified Ohua.Types as OT
import qualified One4All.Lang as O4A
import One4All.Quote

import Ohua.Backend.ArcGen (genInArcVariable, genOutArcVariable)
import Ohua.Backend.Util

genTasks :: OC.OutGraph -> O4A.Expr
-- TODO: filter for sfns here
genTasks (OC.OutGraph ops arcs retArc) = undefined

genTask :: OC.Operator -> OC.OutGraph -> O4A.Expr
genTask (OC.Operator id binding) (OC.OutGraph ops arcs retArc)
    -- generate a list of input arc names for the current sfn and zip the
    -- variable names with the original arcs, also locate the ctrl port if there
    -- is any
 = do
    let inArcs = genInArcList id arcs
    let zippedInArcs = onDemandClone (zip (getInArcs id arcs) inArcs)
    let ctrlPort = findControlInput id arcs
    -- generate a list of output arcs and fold them into a o4a expr
    let outArcs = genOutArcList id arcs
    let sendTree = generateSend outArcs arcs id (OC.operator retArc)
    -- construct sfn invocation and stitch together all parts
    let sf = OT.unwrap (OT.qbName binding)
    let drainArcs = map snd (filter (\(arc, _) -> isNotEnvArc arc) zippedInArcs)
    let drainInpTree = generateRecv drainArcs
    let callArgs =
            foldr
                (\num tree ->
                     let var = "inp" ++ show num
                      in [o4a| inp tree |])
                -- TODO: Needs start expr to fold on right here
                ([0 .. (length drainArcs)] :: [Int])
    let sfnCall = [o4a| let result = sf callArgs in sendTree |]
    if length drainArcs > 0
        then case ctrlPort of
                 Just port -> O4A.Binding (O4A.V "foo")
                 Nothing -> O4A.Binding (O4A.V "foo")
        else case ctrlPort of
                 Just port -> O4A.Binding (O4A.V "foo")
                 Nothing -> O4A.Binding (O4A.V "foo")

-- TODO: this function shall convert the list and add cloning to env arcs as needed
onDemandClone :: [(OC.Arc envExpr, O4A.Var)] -> [(OC.Arc envExpr, O4A.Expr)]
onDemandClone = undefined

genInArcList :: OT.FnId -> [OC.Arc envExpr] -> [O4A.Var]
-- TODO: environment variables handling as in `generate_in_arcs_vec`
genInArcList opId arcs =
    let inArcs = sortOn (OC.operator . OC.target) (getInArcs opId arcs)
     in map (\arc ->
                 case OC.source arc of
                     (OC.LocalSource _) -> genInArcVariable arc
                     (OC.EnvSource _) -> undefined)
            (filter (\(OC.Arc (OC.Target _ idx) _) -> idx > (-1)) inArcs)

genOutArcList :: OT.FnId -> [OC.Arc envExpr] -> [O4A.Var]
genOutArcList opId arcs = map genOutArcVariable (getOutArcs opId arcs)

findControlInput :: OT.FnId -> [OC.Arc envExpr] -> Maybe O4A.Var
findControlInput opId arcs =
    genInArcVariable <$>
    find (\(OC.Arc (OC.Target op idx) _) -> op == opId && idx == (-1)) arcs

-- |This function generates the `send` invocations that form the final part of
-- every task tree. It takes the generated variables for each data packet and
-- the accompanying list of arcs used to identify the correct sending port.
generateSend :: [O4A.Var] -> [OC.Arc envExpr] -> OT.FnId -> OT.FnId -> O4A.Expr
generateSend [] _ opId finalId
    | opId == finalId = [o4a| send resultSnd result |]
    | otherwise = [o4a| () |]
generateSend [out] _ _ _ = [o4a| send out result |]
generateSend (out:outs) (a:arcs) opId finalId =
    case OC.source a of
        (OC.LocalSource (OC.Target _ idx)) ->
            let tree = generateSend outs arcs opId finalId
             in [o4a| let resClone = clone result in
             let r = nth idx resClone in
             let s = send out in tree |]
        (OC.EnvSource _) -> undefined

-- |Takes a list of arc name variables and wraps each one into a `recv`
-- instruction to pull on each arc and stitches all expressions together into a
-- single expr
generateRecv :: [O4A.Expr] -> O4A.Expr
generateRecv arcs =
    foldr
        (\(num, arc) tree ->
             let var = "inp" ++ show num
              in [o4a| let $var:var = recv arc in
                     tree |])
        (O4A.Binding (O4A.V "toBeCompleted"))
        (zip [0 ..] arcs :: [(Int, O4A.Expr)])
generateRecv [] = (O4A.Binding (O4A.V "toBeCompleted"))
