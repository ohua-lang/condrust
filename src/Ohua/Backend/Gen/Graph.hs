module Ohua.Backend.Gen.Graph where

import Universum

import Ohua.Backend.Gen.Arc 
import Ohua.Backend.Gen.Node
import Ohua.Backend.DAGLang as DL
import Ohua.Backend.Types
import Ohua.Backend.Util

genCode :: DFExpr -> CompM DAGExpr
genCode graph = do
    nodesCode <- generateNodeCode graph
    nodesAndArcsCode <- generateArcCode graph nodesCode
    

    -- TODO generate code for Ohua ops (ctrls, recurs, nths) if necessary

    -- TODO generate result arc and lookup
