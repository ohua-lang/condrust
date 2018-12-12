module Ohua.Backend.CodeGen where

import Universum

import qualified Ohua.DFGraph as OC
import qualified One4All.Lang as O4A

import Ohua.Backend.ArcGen (genArcs)
import Ohua.Backend.TaskGen (genTasks)
import Ohua.Backend.Util

genCode :: OC.OutGraph -> O4A.Expr
genCode graph = do
    let arcCode = genArcs graph
    arcCode
