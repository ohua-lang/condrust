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
    deriving (Eq, Functor)

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

fuseSTCLang :: TCProgram Channel (Fusable FusedCtrl) -> TCProgram Channel (Fusable FusedCtrl)
fuseSTCLang (TCProgram chans resultChan exprs) = 
    let noSTC = filter (isNothing . findSTCLang) exprs
        fused = go stclangs
    in TCProgram chans resultChan $ noSTC ++ fused
    where
        go stcs = 
            let sourceAndTarget = map (\stc -> (findSource stc, stc)) stcs
                (toFuse, rest) = split sourceAndTarget
                fused = map (Control . uncurry fuse) toFuse
            in if null rest
                then fused
                else fused ++ go rest

        fuse (Control c) stc = fuseSTCSMap stc c
        -- Now this seems to be a very interesting thing to be able to do with Liquid Haskell:
        -- In the definition of a language I want to have variables and abstract over them.
        -- How can I define constraints for the inputs to certain nodes nevertheless?
        -- That is: How can I transport a type constraint through the variable definition in a
        -- language/expression data type?
        fuse _ _ = error "Invariant broken: Trying to fuse non-STCLangSMap!"

        split xs = 
            let tgts = HS.fromList $ map snd xs
            in partitionEithers $
                map 
                    (\e@(source, target) -> 
                        case source of
                            STC s -> if not $ HS.member s tgts
                                        then Left e
                                        else Right target
                            _ -> Right target)
                    xs
    
        findSource :: STCLangSMap -> Fusable FusedCtrl
        findSource (STCLangSMap _ _ inp _) = 
            case filter (isSource inp) exprs of
                    [src] -> src
                    _ -> error "Invariant broken: every STC has exactly one source by definition!"
        
        stclangs = mapMaybe findSTCLang exprs
        findSTCLang (STC s) = Just s
        findSTCLang _ = Nothing
        isSource inp (STC (STCLangSMap _ _ _ outp)) = outp == inp
        isSource inp (Control (FusedCtrl _ _ _ _ _ outs)) = 
            HS.member inp $ HS.fromList $ map (\(Emit c _) -> c) outs
        isSource inp (Unfusable _) = False
        isSource inp (Fun PureFusable{}) = False
        isSource inp (Fun (STFusable _ _ _ _ send)) =
            maybe False (\(Emit outp _) -> outp == inp) ((\f -> f "0") <$> send)

