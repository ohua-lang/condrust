{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Backend.Fusion where

import Ohua.Commons.Prelude

import Ohua.Backend.Operators hiding (Fun, Fusable, Unfusable)
import Ohua.Backend.Operators.SMap as SMap
import qualified Ohua.Backend.Operators.Function as F
import qualified Ohua.Backend.Operators.State as S (Fuse(Fusable))
import Ohua.Backend.Lang
import Ohua.Backend.Types
import Ohua.Backend.Config

import qualified Data.HashSet as HS
import qualified Data.Map.Ordered as OrdMap

import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as DF
import Data.Set.Ordered ((\\))

data Fusable embExpr ty ctrl0 ctrl1 
    = Fun (FusableFunction embExpr ty)
    | STC (STCLangSMap 'S.Fusable embExpr ty)
    | Control (Either ctrl0 ctrl1)
    | Recur (RecFun embExpr ty)
    | SMap (Op embExpr ty)
    | Unfusable (TaskExpr embExpr ty) -- TODO this is only here until the below was implemented properly
    --  IfFun
    --  Select
    deriving (Eq, Functor, Generic)

deriving instance (Show ty, Show ctrl0, Show ctrl1, Show embExpr) => Show (Fusable embExpr ty ctrl0 ctrl1)

instance (Hashable ctrl0, Hashable ctrl1) => Hashable (Fusable embExpr ty ctrl0 ctrl1)

instance Bifunctor (Fusable embExpr ty) where
    first f (Control (Left l)) = Control $ Left $ f l
    first _ (Control (Right c)) = Control $ Right c
    first _ (Fun fun) = Fun fun
    first _ (STC s) = STC s
    first _ (Recur r) = Recur r
    first _ (Unfusable u) = Unfusable u
    first _ (SMap s) = SMap s
    second = fmap

type FusableExpr embExpr ty = Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty) 

fuse :: (ErrAndLogM m, Show ty, Show embExpr)
     => Options
     -> Namespace (TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty)  embExpr  (Fusable  embExpr  ty (VarCtrl  embExpr  ty) (LitCtrl embExpr ty))) anno (OhuaType ty 'Resolved)
     -> m (Namespace (TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty)  embExpr  (TaskExpr embExpr ty)) anno (OhuaType ty 'Resolved))
fuse options ns =

    return $ ns & algos %~ map (\algo -> algo & algoCode %~ go)
    where
        go :: (Show ty, Show embExpr) => TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty)  embExpr  (Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty))
           -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty)  embExpr  (TaskExpr embExpr ty)
        go = evictUnusedChannels . concludeFusion . (fuseFunctions options) . fuseStateThreads . fuseSMaps
{-
removeIdFun :: TCProgram (Channel embExpr ty1) (Com 'Recv embExpr ty1) (TaskExpr embExpr ty1) -> TCProgram (Channel embExpr ty1) (Com 'Recv embExpr ty1) (TaskExpr embExpr ty1)
removeIdFun (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ map idTraversal exprs
    where 
        idTraversal expr = traverse remID expr
        remID = id
-}


concludeFusion ::(Show ty, Show embExpr) => TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
               -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (TaskExpr embExpr ty)
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
evictUnusedChannels :: TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (TaskExpr embExpr ty)
                    -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (TaskExpr embExpr ty)
evictUnusedChannels (TCProgram chans resultChan exprs) =
    let findChannels e = [ chan | ReceiveData chan <- universe e]
        usedChans = HS.fromList $ concatMap findChannels exprs      -- Order?: We don't need order here as the set is just a filter
        chans' = NE.filter (`HS.member` usedChans) chans
    in TCProgram (resultChan :| chans') resultChan exprs


fuseFunctions ::(Show ty, Show embExpr) => Options
              -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
              -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
fuseFunctions Options{stateInitFusion=False} t = t
fuseFunctions Options{stateInitFusion=True} (TCProgram chans resultChan exprs) =
  let
    exprs' = map funClassify exprs
    funs = lefts exprs'
    other = rights exprs'
    (fusedFuns, unfusedFuns) = go funs
    fusedFuns' = trace ("Fused funs: \n" <> show fusedFuns) map (Unfusable . genFusedFun) $ catMaybes fusedFuns
    unfusedFuns' = trace ("\nUnfused funs: \n" <> show unfusedFuns) map Fun unfusedFuns
  in
    TCProgram chans resultChan (other ++ fusedFuns' ++ unfusedFuns')
  where
    funClassify :: Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty) ->
                   Either (FusableFunction embExpr ty) (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
    funClassify (Fun f) = Left f
    funClassify e = Right e

    go :: (Show embExpr) => [FusableFunction embExpr ty] -> ([Maybe (FusedFun embExpr ty)], [FusableFunction embExpr ty])
    go [] = ([],[])
    go (f : fs) =
      let (fusedFun  , fs' ) = fuseFunction f fs
          (fusedFuns , fs'') = go fs'
          fs''' = case fusedFun of
                    Nothing -> f : fs''
                    a -> fs''
     in (fusedFun : fusedFuns, fs''')

    fuseFunction :: (Show embExpr) => FusableFunction embExpr ty -> [FusableFunction embExpr ty] -> (Maybe (FusedFun embExpr ty), [FusableFunction embExpr ty])
    fuseFunction f [] = (Nothing, [])
    fuseFunction f (g : gs) =
      case F.fuseFuns f g of
        Just f' -> (Just f', gs)
        Nothing -> case F.fuseFuns g f of
                     Just g' -> (Just g', gs)
                     Nothing -> second ([g] ++ ) $ fuseFunction f gs

fuseStateThreads :: (Show embExpr) => 
                TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty))
                -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
fuseStateThreads = fuseSTCLang . fuseCtrls . mergeCtrls

mergeCtrls :: forall ty embExpr . (Show embExpr) => 
              TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty))
           -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FunCtrl embExpr ty) (LitCtrl embExpr ty))
mergeCtrls (TCProgram chans resultChan exprs) =
    let (ctrls',mergedCtrls) = mergeLevel funReceives (OrdMap.fromList ctrls) [f | (Fun f) <- exprs]
        mergedCtrls' =
            mergedCtrls ++ mergeNextLevel (NE.toList . ctrlReceives) ctrls' mergedCtrls
        mergedCtrls'' = map (Control . Left) mergedCtrls' :: [Fusable embExpr ty (FunCtrl embExpr ty) (LitCtrl embExpr ty)]
        noCtrlExprs = filter (isNothing . findCtrl) exprs
    in TCProgram chans resultChan $
        foldl
            (\cs c -> first toFunCtrl c : cs)  -- There is no ctrl here but this converts actually from Fusable VarCtrl to Fusable FunCtrl
            mergedCtrls''
            noCtrlExprs
    where
        mergeNextLevel :: (FunCtrl embExpr ty -> [Com 'Recv embExpr ty])
                        -> OrdMap.OMap (OutputChannel embExpr ty) (VarCtrl embExpr ty)
                        -> [FunCtrl embExpr ty]
                        -> [FunCtrl embExpr ty]
        mergeNextLevel receives cs mcs =
            let (cs',mcs') = mergeLevel receives cs mcs
            in if null mcs'
                then map (toFunCtrl . snd) $ OrdMap.assocs cs'
                else mcs' <> mergeNextLevel receives cs' mcs'

        -- TODO: refactor to use a state monad instead for better readability
        mergeLevel :: (b -> [Com 'Recv embExpr ty])
                    -> OrdMap.OMap (OutputChannel embExpr ty) (VarCtrl embExpr ty)
                    -> [b]
                    -> (OrdMap.OMap (OutputChannel embExpr ty) (VarCtrl embExpr ty), [FunCtrl embExpr ty])
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
        findCtrl :: Fusable embExpr ty (VarCtrl embExpr ty) a -> Maybe (OutputChannel embExpr ty, VarCtrl embExpr ty)
        findCtrl (Control (Left c@(VarCtrl _ (out,_)))) = Just (out, c)
        findCtrl _ = Nothing
        ctrls :: [(OutputChannel embExpr ty, VarCtrl embExpr ty)]
        ctrls = mapMaybe findCtrl exprs

fuseCtrls :: forall ty embExpr. (Show embExpr) => 
             TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FunCtrl embExpr ty) (LitCtrl embExpr ty))
          -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
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

        split :: [Fusable embExpr ty (FunCtrl embExpr ty) (LitCtrl embExpr ty)]
              -> ( [Either (FunCtrl embExpr ty) (LitCtrl embExpr ty)]
                 , [Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty)])
        split = partitionEithers .
                map (\case
                        (Control c) -> Left c
                        -- type conversion from Fusable FunCtrl to Fusable FusedFunCtrl
                        (Fun f) -> Right (Fun f)
                        (STC s) -> Right (STC s)
                        (Recur r) -> Right (Recur r)
                        (SMap s) -> Right $ SMap s
                        (Unfusable u) -> Right (Unfusable u))
        srcsAndTgts :: [Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty)]
                    -> [Either (FunCtrl embExpr ty) (LitCtrl embExpr ty)]
                    -> [ (Either (FunCtrl embExpr ty) (LitCtrl embExpr ty)
                       , Either (FusableFunction embExpr ty) (FusedFunCtrl embExpr ty))]
        srcsAndTgts es = mapMaybe (`findTarget` es)


        fuseIt :: Either (FunCtrl embExpr ty) (LitCtrl embExpr ty)
               -> Either (FusableFunction embExpr ty) (FusedFunCtrl embExpr ty)
               -> Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty)
        fuseIt (Left ctrl) (Left f) = Control $ Left $ fuseFun ctrl f
        fuseIt (Left ctrl) (Right c) = Control $ Left $ fuseCtrl ctrl c
        fuseIt (Right ctrl) (Left c) = Control $ Right $ fuseLitCtrlIntoFun ctrl c
        fuseIt (Right ctrl) (Right c) = Control $ Right $ fuseLitCtrlIntoCtrl ctrl c


        findTarget :: Either (FunCtrl embExpr ty) (LitCtrl embExpr ty)
                   -> [Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty)]
                   -> Maybe ( Either (FunCtrl embExpr ty) (LitCtrl embExpr ty)
                            , Either (FusableFunction embExpr ty) (FusedFunCtrl embExpr ty))
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
        
        
        isTarget :: Com 'Channel embExpr ty -> Fusable embExpr ty (FusedCtrl anno embExpr ty) ctrl1 -> Bool
        isTarget bnd (Control (Left (FusedFunCtrl _ vars _ _))) =
            HS.member bnd $ HS.fromList $
            map ((\(SRecv _ c) -> c) . snd . fromVarReceive) vars
        isTarget bnd (Fun f) =
            HS.member bnd $ HS.fromList $ map (\(SRecv _ c) -> c) $ funReceives f
        isTarget _ _ = False


fuseSTCLang :: forall ty embExpr.
               TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
            -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty))
fuseSTCLang (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ go exprs
    where
        go allEs =
            let stcs = findSTCLangs allEs
                sourceAndTarget = map (\stc -> (findSource allEs stc, stc)) stcs
                (toFuse, unfusable) = split sourceAndTarget
                fused = map (Control . Left . uncurry fuseIt) toFuse
                sTfilter = HS.fromList $ concatMap (\(x,y) -> [x,STC y]) sourceAndTarget
                noFused = filter (not . (`HS.member` sTfilter)) allEs
                all' = noFused ++ fused
            in case unfusable of
                 [] -> all'
                 _  -> if HS.fromList allEs == HS.fromList all'
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
                  STC _s ->
                    Right target
                  _ -> Left e)
            xs

        findSource :: [Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty)] ->  STCLangSMap 'S.Fusable embExpr ty -> Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty)
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

        isSource :: Com 'Channel embExpr ty -> Fusable embExpr ty (FusedFunCtrl embExpr ty) (FusedLitCtrl embExpr ty) -> Bool
        isSource inp (STC (FusableSTCLangSMap _ outp)) = outp == inp
        isSource inp (Control (Left (FusedFunCtrl _ _ _ outs))) =
          HS.member inp $ HS.fromList $ map (\(SSend c _) -> c) outs
        isSource inp (Control (Right (FusedLitCtrl _ _ (Left l)))) =
          isSource inp $ Control $ Left l
        isSource inp (Control (Right (FusedLitCtrl _ _ (Right r)))) = isSource inp $ Fun r
        isSource _inp (Unfusable _) = False
        isSource _inp (Fun PureFusable{}) = False
        isSource inp (Fun (STFusable _ _ _app _ send)) = (== Just inp) send
        isSource _inp (Fun IdFusable{}) = False
        isSource _inp SMap{} = False
        isSource _inp Recur{} = False

-- TODO actually, the fusion of functions into SMap is context-dependent.
--      in case of a nested SMap, the fusion actually kills pipeline parallelism!
fuseSMaps :: forall embExpr ty. (Show embExpr) => 
          TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty))
          -> TCProgram (Channel embExpr ty) (Com 'Recv embExpr ty) embExpr (Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty))
fuseSMaps (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ go 
  where
    go     = let smaps = mapMaybe getSMap exprs
                 exprs' = filter (not . isSMap) exprs
             in foldl getAndDropIt exprs' smaps

    getAndDropIt :: [Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty)]
                 -> Op embExpr ty
                 -> [Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty)]
    getAndDropIt (e:expressions) smap =
      case SMap.getInput smap of
        Just (SRecv _ c) ->
          case getAndDrop smap c e of
            Just task -> Unfusable task : expressions
            Nothing -> e : getAndDropIt expressions smap
        _ -> e:expressions ++ [SMap smap] -- if the input is resolved then we can not drop the smap
    getAndDropIt [] smap = [SMap smap] -- this should actually never happen!

    isSMap SMap{} = True
    isSMap _ = False

    getSMap (SMap s) = Just s
    getSMap _ = Nothing

    getAndDrop :: Op embExpr ty
               -> Com 'Channel embExpr ty
               -> Fusable embExpr ty (VarCtrl embExpr ty) (LitCtrl embExpr ty)
               -> Maybe (TaskExpr embExpr ty)
    getAndDrop smap inp
      (Control (Left (VarCtrl cIn p@(OutputChannel c@(SChan b),_inData) )))
      | c == inp =
          Just $ genFused $ fuseSMap (FunCtrl cIn (p:|[])) $ SMap.fuse b smap
    -- we could of course also fuse if the function returns a tuple or a dispatch but let's
    -- not do this for now.
    getAndDrop smap inp (Fun fun@(PureFusable args _ ((SendResult c@(SChan b)) :| [])))
      | c == inp =
          -- this is fragile because I'm just assuming that every TaskExpr was a literal!
          let smapGen = if hasReceive args then SMap.gen else SMap.gen'
          in Just $ genFunWithCont (smapGen $ SMap.fuse b smap) fun
    getAndDrop smap inp (Fun st@(STFusable sIn dIn app dOut sOut)) =
      case sOut of
        (Just c@(SChan b))
          | c == inp ->
            let send' = genSend $ toFuseFun $ STFusable sIn dIn app dOut Nothing
            in Just $ genFunWithCont (Stmt send' $ SMap.gen' $ SMap.fuse b smap) st
        _ ->
          case dOut of
            [SendResult c@(SChan b)]
              | c == inp ->
                let send' = genSend $ toFuseFun $ STFusable sIn dIn app [] sOut
                in Just $ genFunWithCont  (Stmt send' $ SMap.gen' $ SMap.fuse b smap) st
            _ -> Nothing
    getAndDrop _ _ _ = Nothing
    hasReceive args = all (\case
                                Arg{} -> True
                                (Drop (Left _)) -> True
                                _ -> False
                            ) args

-- REMINDER: (Maybe as a result of fusion) There might be no nodes without input i.e. nodes requiring input might send 
-- before requiring to receive. Hence we can't just sort them by satisfied input requirements

sortByDependency :: TCProgram (Channel embExpr ty1) (Com 'Recv embExpr ty1) embExpr (TaskExpr embExpr ty1) -> TCProgram (Channel embExpr ty1) (Com 'Recv embExpr ty1) embExpr (TaskExpr embExpr ty1)
sortByDependency (TCProgram chans resultChan exprs) = TCProgram chans resultChan $ orderTasks exprs HS.empty
    where
        orderTasks [] _ = []
        orderTasks tasks fulfilledDependencies =
            let taskInOut = map (\t -> (t, findInputs t, findReturns t)) tasks
                toBeScheduled = filter (\(_t, ins, _outs) -> ins `HS.intersection` fulfilledDependencies == ins) taskInOut
                fulfilled = HS.fromList $ concatMap (\(_t, _ins, outs) -> outs) toBeScheduled
                scheduled = map (\(t, _ins, _outs) -> t) toBeScheduled
                remaining = filter (`notElem` scheduled) tasks
            in scheduled ++ orderTasks remaining (fulfilledDependencies `HS.union` fulfilled)

findInputs :: TaskExpr embExpr ty -> HashSet (Com 'Channel embExpr ty)
findInputs expr = HS.fromList $  [ chan | ReceiveData (SRecv _ chan) <- universe expr]

findReturns :: TaskExpr embExpr ty -> [Com 'Channel embExpr ty]
findReturns expr =   [ chan | SendData (SSend chan _) <- universe expr]

