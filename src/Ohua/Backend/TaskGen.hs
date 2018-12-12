module Ohua.Backend.TaskGen where

import Universum

import qualified Ohua.DFGraph as OC
import qualified Ohua.Types as OT
import qualified One4All.Lang as O4A

import Ohua.Backend.ArcGen (genInArcVariable, genOutArcVariable)
import Ohua.Backend.Util

genTasks :: OC.OutGraph -> O4A.Expr
-- TODO: filter for sfns here
genTasks (OC.OutGraph ops arcs retArc) = undefined

genTask :: OT.FnId -> OC.OutGraph -> O4A.Expr
genTask id (OC.OutGraph ops arcs retArc) = do
  let inArcs = genInArcList id arcs
  let zippedInArcs = onDemandClone (zip (getInArcs id arcs) inArcs)
  -- TODO: Continue here: Apply onDemandClone and build the task
  O4A.Binding (O4A.V "foo")

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
