module Ohua.Backend.Gen.Graph where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.Backend.Lang 
import Ohua.Backend.Gen.Arc 
import Ohua.Backend.Gen.Node
import Ohua.Backend.Types

genCode :: CompM m => DFExpr -> m TCExpr
genCode graph = do
    -- TODO generate code for Ohua ops (ctrls, recurs, nths) if necessary
    nodesCode <- generateNodesCode graph
    let arcsCode = generateArcsCode graph
    resultReceive <- generateResultArc graph
    return $ 
        arcsCode $ 
            Run nodesCode resultReceive
