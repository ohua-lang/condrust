{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Backend.Fusion where

import Ohua.Prelude

import Ohua.Backend.Operators hiding (Fun)
import Ohua.Backend.Lang
import Ohua.Backend.Types

import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE


data Fusable ty ctrl0 ctrl1
    = Fun (FusableFunction ty)
    | STC (STCLangSMap ty)
    | Control (Either ctrl0 ctrl1)
    | Unfusable (TaskExpr ty) -- TODO this is only here until the below was implemented properly
    --  IfFun
    --  Select
    --  SMapFun
    --  Collect
    --  RecurFun
    --  UnitFun
    deriving (Eq, Functor, Generic)

instance (Hashable ctrl0, Hashable ctrl1) => Hashable (Fusable ty ctrl0 ctrl1)

instance Bifunctor (Fusable ty) where
    first f (Control (Left l)) = Control $ Left $ f l
    first _ (Control (Right c)) = Control $ Right c
    first _ (Fun fun) = Fun fun
    first _ (STC s) = STC s
    first _ (Unfusable u) = Unfusable u
    second = fmap

type FusableExpr ty = Fusable ty (VarCtrl ty) (LitCtrl ty)

-- TODO add config flag to make fusion optional

fuse :: CompM m => Namespace (TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))) 
             -> m (Namespace (TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)))
fuse ns = 
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ go)
    where 
        go :: TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty)) 
           -> TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
        go = evictUnusedChannels . concludeFusion . fuseStateThreads

concludeFusion :: TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)) 
               -> TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
concludeFusion (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ map go exprs
    where
        go (Fun function) = genFun function
        go (STC stcMap) = genSTCLangSMap stcMap
        go (Control (Left ctrl)) = genFused ctrl
        go (Control (Right ctrl)) = genLitCtrl ctrl
        go (Unfusable e) = e

-- invariant length in >= length out
evictUnusedChannels :: TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty) 
                    -> TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
evictUnusedChannels (TCProgram chans resultChan@(SRecv _ resChan) exprs) = 
    let findChannels e = [ chan | ReceiveData (SRecv _ chan) <- universe e]
        usedChans = HS.fromList $ concatMap findChannels exprs
        chans' = NE.filter (`HS.member` usedChans) chans
    in TCProgram (resChan :| chans') resultChan exprs

fuseStateThreads :: TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty)) 
                 -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
fuseStateThreads = fuseSTCLang . fuseCtrls . mergeCtrls

mergeCtrls :: forall ty.
              TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty)) 
           -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FunCtrl ty) (LitCtrl ty))
mergeCtrls (TCProgram chans resultChan exprs) =
    let (ctrls',mergedCtrls) = mergeLevel funReceives (HM.fromList ctrls) [f | (Fun f) <- exprs]
        mergedCtrls' = 
            mergedCtrls ++ mergeNextLevel (NE.toList . ctrlReceives) ctrls' mergedCtrls
        mergedCtrls'' = map (Control . Left) mergedCtrls' :: [Fusable ty (FunCtrl ty) (LitCtrl ty)]
        noCtrlExprs = filter (isNothing . findCtrl) exprs
    in TCProgram chans resultChan $ 
        foldl 
            (\cs c -> first toFunCtrl c : cs)  -- There is no ctrl here but this converts actually from Fusable VarCtrl to Fusable FunCtrl
            mergedCtrls'' 
            noCtrlExprs
    where
        mergeNextLevel :: (FunCtrl ty -> [Com 'Recv ty]) 
                        -> HashMap (OutputChannel ty) (VarCtrl ty)
                        -> [FunCtrl ty] 
                        -> [FunCtrl ty]
        mergeNextLevel receives cs mcs = 
            let (cs',mcs') = mergeLevel receives cs mcs
            in if null mcs'
                then map (toFunCtrl . snd) $ HM.toList cs'
                else mcs' <> mergeNextLevel receives cs' mcs'

        -- TODO: refactor to use a state monad instead for better readability
        mergeLevel :: (b -> [Com 'Recv ty])
                    -> HashMap (OutputChannel ty) (VarCtrl ty)
                    -> [b] 
                    -> (HashMap (OutputChannel ty) (VarCtrl ty), [FunCtrl ty])
        mergeLevel receives ctrls level = 
            let (ctrls', ctrlsPerFun) = 
                    foldl 
                        (\(ctrls, fs) f -> 
                            let rs = map (\(SRecv _ c) -> OutputChannel c) $ receives f
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
        
        -- assumptions: LitCtrls never need to be merged by their very nature!
        findCtrl :: Fusable ty (VarCtrl ty) a -> Maybe (OutputChannel ty, VarCtrl ty)
        findCtrl (Control (Left c@(VarCtrl _ (out,_)))) = Just (out, c)
        findCtrl _ = Nothing 
        ctrls :: [(OutputChannel ty, VarCtrl ty)]
        ctrls = mapMaybe findCtrl exprs

fuseCtrls :: forall ty.
             TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FunCtrl ty) (LitCtrl ty)) 
          -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
fuseCtrls (TCProgram chans resultChan exprs) = 
    let (ctrls, noFunCtrls) = split exprs
    in TCProgram chans resultChan $ go ctrls noFunCtrls
    where
        go funCtrls noFunCtrls = 
            let sAndT = srcsAndTgts noFunCtrls funCtrls
                fused = map (uncurry fuseIt) sAndT
                orphans = HS.fromList $ 
                    map ((\case Left f -> Fun f; Right c -> Control $ Left c) . snd)
                        sAndT
                noFunCtrls' = filter (not . (`HS.member` orphans)) noFunCtrls
                noFunCtrls'' = fused ++ noFunCtrls'
                pendingFunCtrls = 
                    HS.toList $ foldr (HS.delete . fst) (HS.fromList funCtrls) sAndT
            in if null pendingFunCtrls
                then noFunCtrls''
                else go pendingFunCtrls noFunCtrls''

        split :: [Fusable ty (FunCtrl ty) (LitCtrl ty)] 
              -> ([Either (FunCtrl ty) (LitCtrl ty)], [Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)])
        split = partitionEithers . 
                map (\case 
                        (Control c) -> Left c
                        -- type conversion from Fusable FunCtrl to Fusable FusedFunCtrl
                        (Fun f) -> Right (Fun f)
                        (STC s) -> Right (STC s)
                        (Unfusable u) -> Right (Unfusable u))
        srcsAndTgts :: [Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)] 
                    -> [Either (FunCtrl ty) (LitCtrl ty)] 
                    -> [(Either (FunCtrl ty) (LitCtrl ty), Either (FusableFunction ty) (FusedFunCtrl ty))]
        srcsAndTgts es = mapMaybe (`findTarget` es)
        fuseIt :: Either (FunCtrl ty) (LitCtrl ty)
               -> Either (FusableFunction ty) (FusedFunCtrl ty)
               -> Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)
        fuseIt (Left ctrl) (Left f) = Control $ Left $ fuseFun ctrl f
        fuseIt (Left ctrl) (Right c) = Control $ Left $ fuseCtrl ctrl c
        fuseIt (Right ctrl) (Left c) = Control $ Right $ fuseLitCtrlIntoFun ctrl c
        fuseIt (Right ctrl) (Right c) = Control $ Right $ fuseLitCtrlIntoCtrl ctrl c
        findTarget :: Either (FunCtrl ty) (LitCtrl ty)
                   -> [Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)] 
                   -> Maybe (Either (FunCtrl ty) (LitCtrl ty), Either (FusableFunction ty) (FusedFunCtrl ty))
        findTarget fc es = 
            let chan = case fc of
                        (Left (FunCtrl _ outsAndIns)) -> unwrapChan $ fst $ NE.head outsAndIns
                        (Right (LitCtrl _ (OutputChannel out,_))) -> out
            in case filter (isTarget chan) es of
                [] -> Nothing
                [target] -> case target of
                                (Fun f) -> Just (fc, Left f)
                                (Control (Left c)) -> Just (fc, Right c)
                                _ -> error "Invariant broken!"
                _ -> error "Invariant broken: a control always has exactly one target!"
        isTarget :: Com 'Channel ty -> Fusable ty (FusedCtrl anno ty) ctrl1 -> Bool
        isTarget bnd (Control (Left (FusedFunCtrl _ vars _ _))) = 
            HS.member bnd $ HS.fromList $ NE.toList $ 
            NE.map ((\(SRecv _ c) -> c) . snd . fromVarReceive) vars
        isTarget bnd (Fun f) = 
            HS.member bnd $ HS.fromList $ map (\(SRecv _ c) -> c) $ funReceives f
        isTarget _ _ = False

fuseSTCLang :: forall ty.
               TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)) 
            -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
fuseSTCLang (TCProgram chans resultChan exprs) = 
    let noSTC = filter (isNothing . findSTCLang) exprs
        fused = go stclangs
    in TCProgram chans resultChan $ noSTC ++ fused
    where
        go stcs = 
            let sourceAndTarget = map (\stc -> (findSource stc, stc)) stcs
                (toFuse, rest) = split sourceAndTarget
                fused = map (Control . Left . uncurry fuseIt) toFuse
            in if null rest
                then fused
                else fused ++ go rest

        fuseIt (Control (Left c)) stc = fuseSTCSMap stc c
        fuseIt (Control (Right _c)) _stc = error "Invariant broken: A contextified literal does not expose any state (because then the state would have been contextified)."
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
    
        findSource :: STCLangSMap ty -> Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)
        findSource (STCLangSMap _ _ (SRecv _ inp) _) = 
            case filter (isSource inp) exprs of
                    [src] -> src
                    _ -> error "Invariant broken: every STC has exactly one source by definition!"
        
        stclangs = mapMaybe findSTCLang exprs
        findSTCLang (STC s) = Just s
        findSTCLang _ = Nothing
        
        isSource :: Com 'Channel ty -> Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty) -> Bool
        isSource inp (STC (STCLangSMap _ _ _ outp)) = outp == inp
        isSource inp (Control (Left (FusedFunCtrl _ _ _ outs))) = 
            HS.member inp $ HS.fromList $ map (\(SSend c _) -> c) outs
        isSource inp (Control (Right (FusedLitCtrl _ (OutputChannel out,_) _))) = inp == out
        isSource _inp (Unfusable _) = False
        isSource _inp (Fun PureFusable{}) = False
        isSource inp (Fun (STFusable _ _ _ _ send)) = (== Just inp) send

