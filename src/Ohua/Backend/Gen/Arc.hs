{-# LANGUAGE QuasiQuotes #-}

module Ohua.Backend.Gen.Arc where

import Ohua.Prelude

import qualified Ohua.DFGraph as OC
import Ohua.DFLang.Lang
import Ohua.DFLang.Util

import Ohua.Backend.Util

generateArcsCode :: DFExpr -> TCExpr -> TCExpr
generateArcsCode graph cont = 
    foldr (\e c -> e c) cont $
    concat $ 
    flip map (letExprs graph) $ \letExpr ->
        flip map (output letExpr) $ \out ->
            let numUsages = length $ findUsages out graph
            in Let (VarP out) (Channel numUsages)

generateResultArc :: CompM m => DFExpr -> m TCExpr
generateResultArc graph = 
    let retVar = returnVar graph
    case findUsages retVar graph of
        0 -> return $ Receive 0 $ Var retVar 
        _ -> throwError "Unsupported: use of final result elsewhere in the code."
