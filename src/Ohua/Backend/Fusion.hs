module Ohua.Backend.Fusion where

import Ohua.Prelude

import Ohua.Backend.Operators
import Ohua.Backend.Lang
import Ohua.Backend.Types

import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE


data Fusable ctrl
    = Fun FusableFunction
    | STC STCLangSMap
    | Control ctrl
    | Unfusable TaskExpr -- TODO this is only here until the below was implemented properly
    --  IfFun
    --  Select
    --  SMapFun
    --  Collect
    --  RecurFun
    --  UnitFun
    deriving (Functor)

-- TODO add config flag
fuse :: CompM m => Namespace (TCProgram Channel (Fusable VarCtrl)) -> m (Namespace (TCProgram Channel TaskExpr))
fuse ns = 
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ go)
    where 
        go :: TCProgram Channel (Fusable VarCtrl) -> TCProgram Channel TaskExpr
        go = evictUnusedChannels . concludeFusion

concludeFusion :: TCProgram Channel (Fusable FunCtrl) -> TCProgram Channel TaskExpr
concludeFusion (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ map go exprs
    where
        go (Fun function) = genFun function
        go (STC stcMap) = genSTCLangSMap stcMap
        go (Control ctrl) = genCtrl ctrl
        go (Unfusable e) = e

-- invariant length in >= length out
evictUnusedChannels :: TCProgram Channel TaskExpr -> TCProgram Channel TaskExpr
evictUnusedChannels (TCProgram chans resultChan exprs) = 
    let findBnds e = [ chan | Receive _ chan <- universe e]
        usedChans = HS.fromList $ concatMap findBnds exprs
        chans' = filter (\(Channel chan _) -> chan `HS.member` usedChans) chans
    in TCProgram chans' resultChan exprs

fuseStateThreads :: TCProgram Channel (Fusable VarCtrl) -> TCProgram Channel (Fusable FusedCtrl)
fuseStateThreads (TCProgram chans resultChan exprs) = undefined

mergeCtrls :: TCProgram Channel (Fusable VarCtrl) -> TCProgram Channel (Fusable FunCtrl)
mergeCtrls (TCProgram chans resultChan exprs) =
    let (ctrls',mergedCtrls) = mergeLevel funReceives (HM.fromList ctrls) [f | (Fun f) <- exprs]
        mergedCtrls' = 
            mergedCtrls ++ mergeNextLevel (NE.toList . ctrlReceives) ctrls' mergedCtrls
        mergedCtrls'' = map Control mergedCtrls
        noCtrlExprs = filter (isNothing . findCtrl) exprs
    in TCProgram chans resultChan $ 
        foldl 
            (\cs c -> fmap toFunCtrl c : cs)  -- There is no ctrl here but this converts actually from Fusable VarCtrl to Fusable FunCtrl
            mergedCtrls'' 
            noCtrlExprs
    where
        mergeNextLevel :: (FunCtrl -> [Binding]) 
                        -> HashMap OutputChannel VarCtrl 
                        -> [FunCtrl] 
                        -> [FunCtrl]
        mergeNextLevel receives cs mcs = 
            let (cs',mcs') = mergeLevel receives cs mcs
            in if null mcs'
                then map (toFunCtrl . snd) $ HM.toList cs'
                else mcs' <> mergeNextLevel receives cs' mcs'

        -- TODO: refactor to use a state monad
        mergeLevel :: (b -> [Binding])
                    -> HashMap OutputChannel VarCtrl 
                    -> [b] 
                    -> (HashMap OutputChannel VarCtrl, [FunCtrl])
        mergeLevel receives ctrls level = 
            let (ctrls', ctrlsPerFun) = 
                    foldl 
                        (\(ctrls, fs) f -> 
                            let rs = receives f
                                fs' = mapMaybe (`HM.lookup` ctrls) rs
                                ctrls' = foldl (flip HM.delete) ctrls rs
                            in (ctrls', fs':fs))
                        (ctrls, [])
                        level
                ctrlsPerFun' = map NE.fromList $ filter (not . null) ctrlsPerFun
                mergedCtrls = 
                    map (\cs -> 
                        foldl 
                            (\fc vc -> merge vc $ Right fc) 
                            (toFunCtrl $ NE.head cs) 
                            $ NE.tail cs) 
                        ctrlsPerFun'
            in (ctrls', mergedCtrls)
         
        findCtrl (Control c@(Ctrl _ _ (Identity (out,_)) _ _ _)) = Just (out, c)
        findCtrl _ = Nothing 
        ctrls :: [(OutputChannel, VarCtrl)]
        ctrls = mapMaybe findCtrl exprs
