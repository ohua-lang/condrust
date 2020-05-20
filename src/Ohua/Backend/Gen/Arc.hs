module Ohua.Backend.Gen.Arc where

import Ohua.Prelude

import qualified Ohua.DFGraph as OC
import Ohua.DFLang.Lang
import Ohua.DFLang.Util

import Ohua.Backend.TCLang
import Ohua.Backend.Types


generateArcsCode :: DFExpr -> TCExpr -> TCExpr
generateArcsCode graph cont = 
    foldr (\e c -> e c) cont $
    concat $ 
    flip map (letExprs graph) $ \letExpr ->
        flip map (output letExpr) $ \out ->
            let numUsages = length $ findUsages out $ letExprs graph
            in Let (Var $ unwrap out) (Channel numUsages)

generateResultArc :: CompM m => DFExpr -> m TCExpr
generateResultArc graph = 
    let retVar = returnVar graph
    in case length $ findUsages retVar $ letExprs graph of
        0 -> return $ Receive 0 $ Var $ unwrap retVar 
        _ -> throwError "Unsupported: use of final result elsewhere in the code."
