module Ohua.Backend.Fusion where

import Ohua.Prelude

import Ohua.Backend.Operators
import Ohua.Backend.Lang
import Ohua.Backend.Types

import qualified Data.HashSet as HS


data Fusable 
    = Fun FusableFunction
    | STC STCLangSMap
    | Control Ctrl
    | FusedControl FusedCtrl
    | Unfusable TaskExpr -- TODO this is only here until the below was implemented properly
    --  IfFun
    --  Select
    --  SMapFun
    --  Collect
    --  RecurFun
    --  UnitFun

-- TODO add config flag
fuse :: CompM m => Namespace (TCProgram Channel Fusable) -> m (Namespace (TCProgram Channel TaskExpr))
fuse ns = 
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ go)
    where 
        go :: TCProgram Channel Fusable -> TCProgram Channel TaskExpr
        go = evictUnusedChannels . concludeFusion

concludeFusion :: TCProgram Channel Fusable -> TCProgram Channel TaskExpr
concludeFusion (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ map go exprs
    where
        go (Fun function) = genFun function
        go (STC stcMap) = genSTCLangSMap stcMap
        go (Control ctrl) = genCtrl ctrl
        go (FusedControl ctrl) = genFused ctrl
        go (Unfusable e) = e

-- invariant length in >= length out
evictUnusedChannels :: TCProgram Channel TaskExpr -> TCProgram Channel TaskExpr
evictUnusedChannels (TCProgram chans resultChan exprs) = 
    let findBnds e = [ chan | Receive _ chan <- universe e]
        usedChans = HS.fromList $ concatMap findBnds exprs
        chans' = filter (\(Channel chan _) -> chan `HS.member` usedChans) chans
    in TCProgram chans' resultChan exprs

fuseStateThreads :: TCProgram Channel Fusable -> TCProgram Channel TaskExpr
fuseStateThreads = undefined