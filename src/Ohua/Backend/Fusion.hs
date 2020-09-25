module Ohua.Backend.Fusion where

import Ohua.Prelude

import Ohua.Backend.Operators
import Ohua.Backend.Lang
    ( TaskExpr(Receive), Channel(..), Send(Emit), Recv(Recv) )
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
    deriving (Show, Eq, Functor, Generic)

instance (Hashable ctrl) => Hashable (Fusable ctrl)

type FusableExpr = Fusable VarCtrl

-- TODO add config flag to make fusion optional

fuse :: CompM m => Namespace (TCProgram Channel (Fusable VarCtrl)) -> m (Namespace (TCProgram Channel TaskExpr))
fuse ns = 
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ go)
    where 
        go :: TCProgram Channel (Fusable VarCtrl) -> TCProgram Channel TaskExpr
        go = evictUnusedChannels . concludeFusion . fuseStateThreads

concludeFusion :: TCProgram Channel (Fusable FusedCtrl) -> TCProgram Channel TaskExpr
concludeFusion (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ map go exprs
    where
        go (Fun function) = genFun function
        go (STC stcMap) = genSTCLangSMap stcMap
        go (Control ctrl) = genFused ctrl
        go (Unfusable e) = e

-- invariant length in >= length out
evictUnusedChannels :: TCProgram Channel TaskExpr -> TCProgram Channel TaskExpr
evictUnusedChannels (TCProgram chans resultChan exprs) = 
    let findBnds e = [ chan | Receive _ chan <- universe e]
        usedChans = HS.fromList $ concatMap findBnds exprs
        chans' = filter (\(Channel chan _) -> chan `HS.member` usedChans) chans
    in TCProgram chans' resultChan exprs

fuseStateThreads :: TCProgram Channel (Fusable VarCtrl) -> TCProgram Channel (Fusable FusedCtrl)
fuseStateThreads = fuseSTCLang . fuseCtrls . mergeCtrls

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

        -- TODO: refactor to use a state monad instead for better readability
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
                ctrlsPerFun' = map erroringNE $ filter (not . null) ctrlsPerFun
                mergedCtrls = 
                    map (\cs -> 
                        foldl 
                            (\fc vc -> merge vc $ Right fc) 
                            (toFunCtrl $ NE.head cs) 
                            $ NE.tail cs) 
                        ctrlsPerFun'
            in (ctrls', mergedCtrls)
        erroringNE = fromMaybe (error "Invariant broken: No controls for function!") . nonEmpty
         
        findCtrl (Control c@(Ctrl _ _ (Identity (out,_)) _ _ _)) = Just (out, c)
        findCtrl _ = Nothing 
        ctrls :: [(OutputChannel, VarCtrl)]
        ctrls = mapMaybe findCtrl exprs

-- FIXME Remove the fused functions/tasks!
fuseCtrls :: TCProgram Channel (Fusable FunCtrl) -> TCProgram Channel (Fusable FusedCtrl)
fuseCtrls (TCProgram chans resultChan exprs) = 
    let (ctrls, noFunCtrls) = split exprs
    in TCProgram chans resultChan $ go ctrls noFunCtrls
    where
        go funCtrls noFunCtrls = 
            let sAndT = srcsAndTgts noFunCtrls funCtrls
                fused = map (uncurry fuseIt) sAndT
                orphans = HS.fromList $ 
                    map ((\case Left f -> Fun f; Right c -> Control c) . snd)
                        sAndT
                noFunCtrls' = filter (not . (`HS.member` orphans)) noFunCtrls
                noFunCtrls'' = fused ++ noFunCtrls'
                pendingFunCtrls = 
                    HS.toList $ foldr (HS.delete . fst) (HS.fromList funCtrls) sAndT
            in if null pendingFunCtrls
                then noFunCtrls''
                else go pendingFunCtrls noFunCtrls''

        split :: [Fusable FunCtrl] -> ([FunCtrl], [Fusable FusedCtrl])
        split = partitionEithers . 
                map (\case 
                        (Control c) -> Left c
                        -- type conversion from Fusable FunCtrl to Fusable FusedCtrl
                        (Fun f) -> Right (Fun f)
                        (STC s) -> Right (STC s)
                        (Unfusable u) -> Right (Unfusable u))
        srcsAndTgts :: [Fusable FusedCtrl] -> [FunCtrl] -> [(FunCtrl, Either FusableFunction FusedCtrl)]
        srcsAndTgts es = mapMaybe (`findTarget` es)
        fuseIt :: FunCtrl -> Either FusableFunction FusedCtrl -> Fusable FusedCtrl
        fuseIt ctrl (Left f) = Control $ fuseFun ctrl f
        fuseIt ctrl (Right c) = Control $ fuseCtrl ctrl c
        findTarget:: FunCtrl -> [Fusable FusedCtrl] -> Maybe (FunCtrl, Either FusableFunction FusedCtrl)
        findTarget fc@(Ctrl _ _ outsAndIns _ _ _) es = 
            let chan = fst $ NE.head outsAndIns
            in case filter (isTarget chan) es of
                [] -> Nothing
                [target] -> case target of
                                (Fun f) -> Just (fc, Left f)
                                (Control c) -> Just (fc, Right c)
                                _ -> error "Invariant broken!"
                _ -> error "Invariant broken: a control always has exactly one target!"
        isTarget bnd (Control (FusedCtrl _ vars _ _ _ _)) = 
            HS.member bnd $ HS.fromList $ NE.toList $ 
            NE.map ((\(Recv _ b) -> b) . snd . from) vars
        isTarget bnd (Fun f) = HS.member bnd $ HS.fromList $ funReceives f
        isTarget _ _ = False

fuseSTCLang :: TCProgram Channel (Fusable FusedCtrl) -> TCProgram Channel (Fusable FusedCtrl)
fuseSTCLang (TCProgram chans resultChan exprs) = 
    let noSTC = filter (isNothing . findSTCLang) exprs
        fused = go stclangs
    in TCProgram chans resultChan $ noSTC ++ fused
    where
        go stcs = 
            let sourceAndTarget = map (\stc -> (findSource stc, stc)) stcs
                (toFuse, rest) = split sourceAndTarget
                fused = map (Control . uncurry fuseIt) toFuse
            in if null rest
                then fused
                else fused ++ go rest

        fuseIt (Control c) stc = fuseSTCSMap stc c
        -- Now this seems to be a very interesting thing to be able to do with Liquid Haskell:
        -- In the definition of a language I want to have variables and abstract over them.
        -- How can I define constraints for the inputs to certain nodes nevertheless?
        -- That is: How can I transport a type constraint through the variable definition in a
        -- language/expression data type?
        fuseIt _ _ = error "Invariant broken: Trying to fuse non-STCLangSMap!"

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

