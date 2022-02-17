{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Backend.Fusion where

import Ohua.Prelude

import Ohua.Backend.Operators hiding (Fun, Fusable, Unfusable)
import Ohua.Backend.Operators.SMap as SMap
import qualified Ohua.Backend.Operators.State as S (Fuse(Fusable))
import Ohua.Backend.Lang
import Ohua.Backend.Types

import qualified Data.HashSet as HS
import qualified Data.Map.Ordered as OrdMap

import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as DF
import Data.Set.Ordered ((\\))

data Fusable ty ctrl0 ctrl1
    = Fun (FusableFunction ty)
    | STC (STCLangSMap 'S.Fusable ty)
    | Control (Either ctrl0 ctrl1)
    | Recur (RecFun ty)
    | SMap (Op ty)
    | Unfusable (TaskExpr ty) -- TODO this is only here until the below was implemented properly
    --  IfFun
    --  Select
    deriving (Eq, Functor, Generic)

instance (Hashable ctrl0, Hashable ctrl1) => Hashable (Fusable ty ctrl0 ctrl1)

instance Bifunctor (Fusable ty) where
    first f (Control (Left l)) = Control $ Left $ f l
    first _ (Control (Right c)) = Control $ Right c
    first _ (Fun fun) = Fun fun
    first _ (STC s) = STC s
    first _ (Recur r) = Recur r
    first _ (Unfusable u) = Unfusable u
    first _ (SMap s) = SMap s
    second = fmap

type FusableExpr ty = Fusable ty (VarCtrl ty) (LitCtrl ty)

-- TODO add config flag to make fusion optional

fuse :: CompM m => Namespace (TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))) anno
             -> m (Namespace (TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)) anno)
fuse ns =
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ go)
    where
        go :: TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))
           -> TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
        go = sortByDependency . evictUnusedChannels . concludeFusion . fuseStateThreads . fuseSMaps



concludeFusion :: TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
               -> TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
concludeFusion (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ map go exprs
    where
        go (Fun function) = genFun function
        go (STC stcMap) = error "invariant broken: pending STC found." -- FIXME catch this at the type-level via a subset type
        go (Control (Left ctrl)) = genFused ctrl
        go (Control (Right ctrl)) = genLitCtrl ctrl
        go (Recur r) = mkRecFun r
        go (SMap s) = SMap.gen s
        go (Unfusable e) = e

-- invariant length in >= length out -- TODO use length-indexed vectors
evictUnusedChannels :: TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
                    -> TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)
evictUnusedChannels (TCProgram chans resultChan exprs) =
    let findChannels e = [ chan | ReceiveData chan <- universe e]
        usedChans = HS.fromList $ concatMap findChannels exprs      -- Order?: We don't need order here as the set is just a filter
        chans' = NE.filter (`HS.member` usedChans) chans
    in TCProgram (resultChan :| chans') resultChan exprs

fuseStateThreads :: TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))
                 -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
fuseStateThreads = fuseSTCLang . fuseCtrls . mergeCtrls

mergeCtrls :: forall ty.
              TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))
           -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FunCtrl ty) (LitCtrl ty))
mergeCtrls (TCProgram chans resultChan exprs) =
    let (ctrls',mergedCtrls) = mergeLevel funReceives (OrdMap.fromList ctrls) [f | (Fun f) <- exprs]
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
                        -> OrdMap.OMap (OutputChannel ty) (VarCtrl ty)
                        -> [FunCtrl ty]
                        -> [FunCtrl ty]
        mergeNextLevel receives cs mcs =
            let (cs',mcs') = mergeLevel receives cs mcs
            in if null mcs'
                then map (toFunCtrl . snd) $ OrdMap.assocs cs'
                else mcs' <> mergeNextLevel receives cs' mcs'

        -- TODO: refactor to use a state monad instead for better readability
        mergeLevel :: (b -> [Com 'Recv ty])
                    -> OrdMap.OMap (OutputChannel ty) (VarCtrl ty)
                    -> [b]
                    -> (OrdMap.OMap (OutputChannel ty) (VarCtrl ty), [FunCtrl ty])
        mergeLevel receives ctrls level =
            let (ctrls', ctrlsPerFun) =
                    foldl
                        (\(ctrls, fs) f ->
                            let rs = map (\(SRecv _ c) -> OutputChannel c) $ receives f
                                fs' = mapMaybe (`OrdMap.lookup` ctrls) rs
                                ctrls' = foldl (flip OrdMap.delete) ctrls rs
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
                    map ((\case Left f -> Fun f; Right c -> Control $ Left c) . snd)    -- Order?: This also need not be sorted, likewise just a ffilter
                        sAndT
                noFunCtrls' = filter (not . (`HS.member` orphans)) noFunCtrls
                noFunCtrls'' = fused ++ noFunCtrls'
                -- pendingFunCtrls = 
                   -- turn funCtrls into a set and delete all items s' that are in sAndT::[(s, T)], than turn it into a list again
                   -- DF.toList $ foldr (HS.delete . fst) (HS.fromList funCtrls) sAndT
                ss' = HS.fromList . map fst $ sAndT
                pendingFunCtrls = filter (not . (`HS.member` ss')) funCtrls
            in if null pendingFunCtrls
                then noFunCtrls''
                else
                 if HS.fromList pendingFunCtrls == HS.fromList funCtrls
                 then error $
                      " There are controls that can not be fused." <>
                      "\nNumber of pending controls: " <> show (length pendingFunCtrls) <>
                      "\nPending controls: " <> show pendingFunCtrls
                 else go pendingFunCtrls noFunCtrls''

        split :: [Fusable ty (FunCtrl ty) (LitCtrl ty)]
              -> ( [Either (FunCtrl ty) (LitCtrl ty)]
                 , [Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)])
        split = partitionEithers .
                map (\case
                        (Control c) -> Left c
                        -- type conversion from Fusable FunCtrl to Fusable FusedFunCtrl
                        (Fun f) -> Right (Fun f)
                        (STC s) -> Right (STC s)
                        (Recur r) -> Right (Recur r)
                        (SMap s) -> Right $ SMap s
                        (Unfusable u) -> Right (Unfusable u))
        srcsAndTgts :: [Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)]
                    -> [Either (FunCtrl ty) (LitCtrl ty)]
                    -> [ (Either (FunCtrl ty) (LitCtrl ty)
                       , Either (FusableFunction ty) (FusedFunCtrl ty))]
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
                   -> Maybe ( Either (FunCtrl ty) (LitCtrl ty)
                            , Either (FusableFunction ty) (FusedFunCtrl ty))
        findTarget fc es =
            let chan = case fc of
                        (Left (FunCtrl _ outsAndIns)) ->
                          unwrapChan $ fst $ NE.head outsAndIns
                        (Right (LitCtrl _ (OutputChannel out,_))) -> out
            in case filter (isTarget chan) es of
                [] -> Nothing
                [target] -> case target of
                                (Fun f) -> Just (fc, Left f)
                                (Control (Left c)) -> Just (fc, Right c)
                                _ -> error "Invariant broken!"
                _ -> error $ "Invariant broken: a control always has exactly one target!\nI was looking for a target for channel: " <> show chan
        isTarget :: Com 'Channel ty -> Fusable ty (FusedCtrl anno ty) ctrl1 -> Bool
        isTarget bnd (Control (Left (FusedFunCtrl _ vars _ _))) =
            HS.member bnd $ HS.fromList $
            map ((\(SRecv _ c) -> c) . snd . fromVarReceive) vars
        isTarget bnd (Fun f) =
            HS.member bnd $ HS.fromList $ map (\(SRecv _ c) -> c) $ funReceives f
        isTarget _ _ = False


fuseSTCLang :: forall ty.
               TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
            -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty))
fuseSTCLang (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ go exprs
    where
        go all =
            let stcs = findSTCLangs all
                sourceAndTarget = map (\stc -> (findSource all stc, stc)) stcs
                (toFuse, unfusable) = split sourceAndTarget
                fused = map (Control . Left . uncurry fuseIt) toFuse
                sTfilter = HS.fromList $ concatMap (\(x,y) -> [x,STC y]) sourceAndTarget
                noFused = filter (not . (`HS.member` sTfilter)) all
                all' = noFused ++ fused
            in case unfusable of
                 [] -> all'
                 _  -> if HS.fromList all == HS.fromList all'
                 then error "endless loop detected. there are ctxtExits that can not be fused."
                 else go all'

        fuseIt (Control (Left c)) stc = fuseSTCSMap stc c
        fuseIt (Control (Right _c)) _stc = error "Invariant broken: A contextified literal does not expose any state (because then the state would have been contextified)."
        -- Now this seems to be a very interesting thing to be able to do with Liquid Haskell:
        -- In the definition of a language I want to have variables and abstract over them.
        -- How can I define constraints for the inputs to certain nodes nevertheless?
        -- That is: How can I transport a type constraint through the variable definition in a
        -- language/expression data type?
        fuseIt (Fun _) _ = error "Invariant broken: Trying to fuse fun!"
        fuseIt (STC _) _ = error "Invariant broken: Trying to fuse STC!"
        fuseIt (Recur _) _ = error "Invariant broken: Trying to fuse recur!"
        fuseIt (SMap _) _ = error "Invariant broken: Trying to fuse smap/collect!"
        fuseIt (Unfusable _) _ = error "Invariant broken: Trying to fuse unfusable!"

        split xs =
          partitionEithers $
          map
            (\e@(source, target) ->
                case source of
                  STC s ->
                    Right target
                  _ -> Left e)
            xs

        findSource :: [Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)] ->  STCLangSMap 'S.Fusable ty -> Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty)
        findSource noneSTCs (FusableSTCLangSMap (SRecv _ inp) _) =
            case filter (isSource inp) noneSTCs of
                    [src] -> src
                    s -> error  $ "Invariant broken: every STC has exactly one source by definition!"
                      <> "\nlength: " <> show (length s)
                      <> "\ninp: " <> show inp
                      <> "\n num exprs: " <> show (length exprs)

        findSTCLangs = mapMaybe findSTCLang

        findSTCLang (STC s) = Just s
        findSTCLang _ = Nothing

        isSource :: Com 'Channel ty -> Fusable ty (FusedFunCtrl ty) (FusedLitCtrl ty) -> Bool
        isSource inp (STC (FusableSTCLangSMap _ outp)) = outp == inp
        isSource inp (Control (Left (FusedFunCtrl _ _ _ outs))) =
          HS.member inp $ HS.fromList $ map (\(SSend c _) -> c) outs
        isSource inp (Control (Right (FusedLitCtrl _ _ (Left l)))) =
          isSource inp $ Control $ Left l
        isSource inp (Control (Right (FusedLitCtrl _ _ (Right r)))) = isSource inp $ Fun r
        isSource _inp (Unfusable _) = False
        isSource _inp (Fun PureFusable{}) = False
        isSource inp (Fun (STFusable _ _ app _ send)) = (== Just inp) send
        isSource inp (Fun IdFusable{}) = False
        isSource inp SMap{} = False
        isSource inp Recur{} = False

-- TODO actually, the fusion of functions into SMap is context-dependent.
--      in case of a nested SMap, the fusion actually kills pipeline parallelism!
fuseSMaps :: forall ty.
          TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))
          -> TCProgram (Channel ty) (Com 'Recv ty) (Fusable ty (VarCtrl ty) (LitCtrl ty))
fuseSMaps (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ go exprs
  where
    go all = let smaps = mapMaybe getSMap exprs
                 exprs' = filter (not . isSMap) exprs
             in foldl getAndDropIt exprs' smaps

    getAndDropIt :: [Fusable ty (VarCtrl ty) (LitCtrl ty)]
                 -> Op ty
                 -> [Fusable ty (VarCtrl ty) (LitCtrl ty)]
    getAndDropIt (e:expressions) smap =
      case SMap.getInput smap of
        Just (SRecv _ c) ->
          case getAndDrop smap c e of
            Just task -> Unfusable task : expressions
            Nothing -> e : getAndDropIt expressions smap
        _ -> e:expressions
    getAndDropIt [] smap = [SMap smap] -- this should actually never happen!

    isSMap SMap{} = True
    isSMap _ = False

    getSMap (SMap s) = Just s
    getSMap _ = Nothing

    getAndDrop :: Op ty
               -> Com 'Channel ty
               -> Fusable ty (VarCtrl ty) (LitCtrl ty)
               -> Maybe (TaskExpr ty)
    getAndDrop smap inp
      (Control (Left (VarCtrl cIn p@(OutputChannel c@(SChan b),inData) )))
      | c == inp =
          Just $ genFused $ fuseSMap (FunCtrl cIn (p:|[])) $ SMap.fuse b smap
    -- we could of course also fuse if the function returns a tuple or a dispatch but let's
    -- not do this for now.
    getAndDrop smap inp (Fun fun@(PureFusable _ _ ((SendResult c@(SChan b)) :| [])))
      | c == inp =
          Just $ genFun' (SMap.gen $ SMap.fuse b smap) $ toFuseFun fun
    getAndDrop smap inp (Fun st@(STFusable sIn dIn app dOut sOut)) =
      case sOut of
        (Just c@(SChan b))
          | c == inp ->
            let send' = genSend $ toFuseFun $ STFusable sIn dIn app dOut Nothing
            in Just $ genFun' (Stmt send' $ SMap.gen' $ SMap.fuse b smap) $ toFuseFun st
        _ ->
          case dOut of
            [SendResult c@(SChan b)]
              | c == inp ->
                let send' = genSend $ toFuseFun $ STFusable sIn dIn app [] sOut
                in Just $ genFun' (Stmt send' $ SMap.gen' $ SMap.fuse b smap) $ toFuseFun st
            _ -> Nothing
    getAndDrop _ _ _ = Nothing

sortByDependency :: TCProgram (Channel ty1) (Com 'Recv ty1) (TaskExpr ty1) -> TCProgram (Channel ty1) (Com 'Recv ty1) (TaskExpr ty1)
sortByDependency (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ orderTasks exprs HS.empty
    where
        orderTasks [] _ = []
        orderTasks tasks fulfilledDependencies =
            let taskInOut = map (\t -> (t, findInputs t, findReturns t)) tasks
                -- TODO: replace intersection by isSubsetOf after updating our HashSet version
                toBeScheduled = filter (\(t, ins, outs) -> ins `HS.intersection` fulfilledDependencies == ins) taskInOut
                fulfilled = HS.fromList $ concatMap (\(t, ins, outs) -> outs) toBeScheduled
                scheduled = map (\(t, ins, outs) -> t) toBeScheduled
                remaining = filter (`notElem` scheduled) tasks
            in scheduled ++ orderTasks remaining (fulfilledDependencies `HS.union` fulfilled)

findInputs :: TaskExpr ty -> HashSet (Com 'Channel ty)
findInputs expr = HS.fromList $  [ chan | ReceiveData (SRecv _ chan) <- universe expr]

findReturns :: TaskExpr ty -> [Com 'Channel ty]
findReturns expr =   [ chan | SendData (SSend chan _) <- universe expr]

