{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds, DeriveGeneric, OverloadedLists, TypeApplications, RankNTypes #-}
module Ohua.Backend.Operators.Control where

import Ohua.Commons.Prelude

import Ohua.Backend.Operators.Common
import Ohua.Backend.Lang as L
import Ohua.Backend.Operators.State
import qualified Ohua.Backend.Operators.Function as F
import qualified Ohua.Backend.Operators.SMap as SMap
import Ohua.Backend.Fusion.Util

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS
import qualified Data.Foldable as DF

newtype  CtrlInput embExpr annot ty = CtrlInput (Com 'Recv embExpr annot ty) deriving (Eq, Show, Generic)

instance Hashable (CtrlInput embExpr annot ty)

-- annotations
data CtrlAnno = Variable | Fun | CtxtLit deriving (Eq, Show, Generic)

data Ctrl (anno::CtrlAnno) (embExpr::Type) (ty::Type) :: Type where
    VarCtrl ::  CtrlInput embExpr annot ty -> (OutputChannel embExpr annot ty, Com 'Recv embExpr annot ty) -> Ctrl 'Variable embExpr annot ty
    -- TODO this is not the right type! It does not transport the 
    -- invariant that all output channels have the same target.
    -- This is what `merge` establishes.
    -- Is this again something that can be created with LiquidHaskell?
    FunCtrl ::  CtrlInput embExpr annot ty -> NonEmpty (OutputChannel embExpr annot ty, Com 'Recv embExpr annot ty) -> Ctrl 'Fun embExpr annot ty
    LitCtrl ::  CtrlInput embExpr annot ty -> (OutputChannel embExpr annot ty, Lit embExpr annot ty Resolved) -> Ctrl 'CtxtLit embExpr annot ty

deriving instance Eq (Ctrl semTy embExpr annot ty)
deriving instance Show (Ctrl semTy embExpr annot ty)

instance Hashable (Ctrl semTy embExpr annot ty) where
    hashWithSalt s (VarCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut
    hashWithSalt s (FunCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut
    hashWithSalt s (LitCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut

type VarCtrl = Ctrl 'Variable
type FunCtrl = Ctrl 'Fun
type LitCtrl = Ctrl 'CtxtLit

ctrlReceives :: FunCtrl embExpr annot ty -> NonEmpty (Com 'Recv embExpr annot ty)
ctrlReceives (FunCtrl _ vars) = map snd vars

toFunCtrl :: VarCtrl embExpr annot ty -> FunCtrl embExpr annot ty
toFunCtrl (VarCtrl ctrlVar var) = FunCtrl ctrlVar (var:|[])

data FusedCtrlAnno = Function | Literal deriving (Eq, Show, Generic)

data FusedCtrl (anno::FusedCtrlAnno) (embExpr::Type) (ty::Type) :: Type where
    -- there is another assumption here that needs to be enforced:
    -- state inputs in NonEmpty VarReceive map to state outputs in [Send]!
    FusedFunCtrl ::  CtrlInput embExpr  ty -> [VarReceive embExpr annot ty] -> TaskExpr embExpr annot ty -> [Com 'Send embExpr annot ty] -> FusedCtrl 'Function embExpr annot ty
    FusedLitCtrl ::  CtrlInput embExpr  ty -> (OutputChannel embExpr  ty, Lit embExpr annot ty Resolved) -> Either (FusedFunCtrl embExpr annot ty) (F.FusableFunction  embExpr annot ty) -> FusedCtrl 'Literal embExpr annot ty

deriving instance Eq (FusedCtrl semTy embExpr annot ty)

type FusedFunCtrl = FusedCtrl 'Function
type FusedLitCtrl = FusedCtrl 'Literal

instance Hashable (FusedCtrl anno embExpr annot ty) where
    hashWithSalt s (FusedFunCtrl cInp inOut comp stateOuts) =
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp `hashWithSalt` stateOuts
    hashWithSalt s (FusedLitCtrl cInp inOut comp) =
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp

fuseSTCSMap :: STCLangSMap 'Fusable embExpr annot ty -> FusedFunCtrl embExpr annot ty -> FusedFunCtrl embExpr annot ty
fuseSTCSMap
    (FusableSTCLangSMap (SRecv _ stateReceive) stateOut)
    (FusedFunCtrl ctrlInput vars comp stateOuts)
    = FusedFunCtrl ctrlInput vars comp stateOuts'
    where
        stateOuts' = map (\(SSend ch d) -> if ch == stateReceive
                                            then SSend stateOut d
                                            else SSend ch d)
                         stateOuts

-- | Fusion of two controls.
--   That is
--
--     +----+     +----+
--     |ctrl| --> |ctrl|
--     +----+     +----+
--
--   becomes
--
--     +---------------+
--     | ctrl --> ctrl |
--     +---------------+
--
fuseCtrl :: forall embExpr annot ty.FunCtrl embExpr annot ty -> FusedFunCtrl embExpr annot ty -> FusedFunCtrl embExpr annot ty
fuseCtrl
    (FunCtrl ctrlInput vars) -- from
    (FusedFunCtrl ctrlInput' vars' comp' stateOuts) -- to
    = FusedFunCtrl
        ctrlInput
        vars''
        (genCtrl' ctrlInput' initVarsExpr' comp' Nothing)
        stateOuts'
    where
        initVarsExpr' = initVarsExpr (map (first unwrapBnd . fromVarReceive) vars') $ NE.toList sendVars
        sendVars = NE.map fst vars
        stateVars =
            HS.fromList $
            mapMaybe (\case StateVar _outChan (SRecv _ b) -> Just b; _ -> Nothing)
            vars'
        vars'' =
            toList $
            map
                ((\(out@(OutputChannel b), re) ->
                    if HS.member b stateVars
                    then StateVar out re
                    else PureVar out re)
                . propagateTypeFromRecv vars)
                vars
        stateOuts' = map g stateOuts
        g (SSend ch (Left d)) =
            let d' = case NE.filter (\(OutputChannel (SChan o),_r) -> o == d) vars of
                        [(OutputChannel (SChan o),_)] -> o
                        _ -> error "invariant broken"
            in SSend ch $ Left d' -- the var that I assign the state to becomes the new data out for the state
        -- TODO(feliix42): Implement this
        g (SSend _ch (Right _l)) = error "Fusing controls with literals is not supported yet (ohua-lang/ohua-backend#22)"
        propagateTypeFromRecv = propagateType . toList . map snd

-- | This takes a function and fuses a control into it.
--   That is
--
--     +---+     +----+
--     |fun| --> |ctrl|
--     +---+     +----+
--
--   becomes
--
--     +--------------+
--     | fun --> ctrl |
--     +--------------+
--
--   As a result, the control takes care of the output.
fuseCtrlIntoFun :: forall embExpr annot ty. F.FusableFunction embExpr annot ty -> FusedFunCtrl embExpr annot ty -> F.FusedFun embExpr annot ty
fuseCtrlIntoFun fun (FusedFunCtrl ctrlIn ins expr outs) =
    case fun of
        (F.PureFusable recvs qb out) ->
            forPure' (F.PureFusable recvs qb . fst) out Nothing ins
        (F.STFusable sRecv recvs qb out sOut) ->
--            let ins' = filterData (maybeToList sOut) ins
--                sOut' = if length ins' < length ins then Nothing else sOut
--             in
--               forPure' (\o -> F.STFusable (F.Arg sRecv) recvs qb o sOut') out sOut' ins'
               forPure' (uncurry $ F.STFusable (F.Arg sRecv) recvs qb) out sOut ins
        (F.IdFusable recv out) ->
            forPure' (F.IdFusable recv . fst) out Nothing []
   where
     forPure' :: {-forall t embExpr.-} (DF.Foldable t, Functor t) =>
       ((t (F.Result embExpr annot ty), Maybe (Com 'Channel embExpr annot ty)) -> F.FusedFunction embExpr annot ty)
       -> t (F.Result embExpr annot ty)
       -> Maybe (Com 'Channel embExpr annot ty)
        -> [VarReceive embExpr annot ty]
        -> F.FusedFun embExpr annot ty
     forPure' upstream usOuts usStateOut dsIns =
          let (downstreamIns, upstreamOuts) = forPure usOuts usStateOut dsIns
              upstream' = upstream upstreamOuts
          in
            F.FusedFun upstream' $
            genFused $
            FusedFunCtrl ctrlIn downstreamIns expr outs

-- | This takes a control and fuses a function into it.
--   That is
--
--     +----+     +---+
--     |ctrl| --> |fun|
--     +----+     +---+
--
--   becomes
--
--     +--------------+
--     | ctrl --> fun |
--     +--------------+
--
--   As a result, the function takes care of the output.
fuseFun :: Show embExpr => FunCtrl embExpr annot ty -> F.FusableFunction embExpr annot ty -> FusedFunCtrl embExpr annot ty
fuseFun (FunCtrl ctrlInput vars) =
    \case
        (F.PureFusable args app res) ->
            FusedFunCtrl
                ctrlInput
                (map (uncurry PureVar . propagateTypeFromArg args) $ toList vars)
                (F.genFun'' $  F.PureFusable (map f args) app res)
                []
        (F.STFusable stateArg args app res stateRes) ->
            FusedFunCtrl
                ctrlInput
                (vars' stateArg args)
                (F.genFused $
                 F.STFusable
                 (f $ F.Arg stateArg)
                 (map f args)
                 app
                 res
                 Nothing)
                (maybe
                    []
                    (\g -> [SSend g $ Left $ unwrapBnd $ fst $ stateVar stateArg])
                    stateRes)
        (F.IdFusable arg res) ->
            FusedFunCtrl
                ctrlInput
                (map (uncurry PureVar . propagateTypeFromArg [arg]) $ toList vars)
                (F.genFun'' $  F.IdFusable (f arg) res)
                []
        where
            f (F.Arg (SRecv _ (SChan ch))) | HS.member (SChan ch) ctrled =
                                             F.Converted $ Var ch
            f (F.Drop (Left (SRecv _ (SChan ch)))) | HS.member (SChan ch) ctrled =
                                                   F.Drop $ Right $ Var ch
            f e  = e
            ctrled = HS.fromList $ toList $ map unwrapChan sendVars
            sendVars = NE.map fst vars
            stateVar (SRecv _ sArg)=
              case NE.filter (\(l, _) -> (unwrapChan l) == sArg) vars of
                [s] -> s
                []  -> error "invariant broken. channel name not found " -- an assumption rooted inside DFLang
                many-> error "invariant broken. channel name not unique " -- an assumption rooted inside DFLang
            vars' sArg args =
                toList $
                NE.map ((\case
                            (bnd, a) | a == sArg -> StateVar bnd a
                            (bnd, a) -> PureVar bnd a)
                        . propagateTypeFromArg args)
                        vars
            propagateTypeFromArg =
              propagateType . mapMaybe (\case (F.Arg s) -> Just s; _ -> Nothing)

fuseSMap :: FunCtrl embExpr annot ty -> SMap.Op embExpr annot ty -> FusedFunCtrl embExpr annot ty
fuseSMap (FunCtrl ctrlInput (( o, inData):|[])) smap = -- invariant
  FusedFunCtrl
    ctrlInput
    [PureVar o inData]
    (SMap.gen' $ SMap.fuse (unwrapBnd o) smap)
    []
fuseSMap other _ = error $ "Compiler Invariant broken. Trying to fuse SMap for " <> show other

propagateType :: [Com 'Recv embExpr annot ty] -> (OutputChannel embExpr annot ty, Com 'Recv embExpr annot ty) -> (OutputChannel embExpr annot ty, Com 'Recv embExpr annot ty)
propagateType args (o@(OutputChannel outChan), r@(SRecv _ rChan)) =
    foldl (\s out -> case out of
                        (SRecv t chan) | chan == outChan -> (o, SRecv t rChan)
                        -- TODO this is actually not possible and should be an invariant because all vars
                        --      of the control have a corresponding arg.
                        _ -> s)
        (o,r)
        args

fuseLitCtrlIntoCtrl :: LitCtrl embExpr annot ty ->  FusedFunCtrl embExpr annot ty -> FusedLitCtrl embExpr annot ty
fuseLitCtrlIntoCtrl (LitCtrl ctrlInp inOut) = FusedLitCtrl ctrlInp inOut . Left

fuseLitCtrlIntoFun :: LitCtrl embExpr annot ty -> F.FusableFunction embExpr annot ty -> FusedLitCtrl embExpr annot ty
fuseLitCtrlIntoFun (LitCtrl ctrlInp outIn) = FusedLitCtrl ctrlInp outIn . Right

{-|
  Now, we can easily merge the controls for a single function, so that the
  (controlled) variables to a single function are guarded by the same control node.
  So this:
           x ---> ctrl1 --------------+
                    ^                 |
                    |                 v
  sig-source -------+       let _ = f x y in g y
        |           |                   ^      ^
        |           v                   |      |
        |  y ---> ctrl2 ----------------+      |
        |  |                                   |
        |  |                                   |
        |  +----> ctrl3 -----------------------+
        |           ^
        |           |
        +-----------+

becomes:
           x --> ctrl(1+2) -------------+
                 ^  ^  |                |
                 |  |  +--------------+ |
              +--+  |                 | |
              |     |                 v v
  sig-source -+-----+       let _ = f x y in g y
              |     |                          ^
              |     v                          |
           y -+-> ctrl3 -----------------------+

  That really just means that I'm removing all signaling code because it is already there.
-}
-- Assumption: _sigSource == _sigSource'
merge :: VarCtrl embExpr annot ty -> Either (VarCtrl embExpr annot ty) (FunCtrl embExpr annot ty) -> FunCtrl embExpr annot ty
merge (VarCtrl ctrlIn var) (Left (VarCtrl _ctrlIn' var')) = FunCtrl ctrlIn [var, var']
merge (VarCtrl ctrlIn var) (Right (FunCtrl _ctrlIn' vars)) = FunCtrl ctrlIn $ var NE.<| vars

genFused ::  FusedFunCtrl embExpr annot ty -> TaskExpr embExpr annot ty
genFused = EndlessLoop . genFused'

genFused' ::  FusedFunCtrl embExpr annot ty -> TaskExpr embExpr annot ty
genFused' (FusedFunCtrl ctrlInput vars comp stateOuts) =
    genCtrl' ctrlInput initVarsExpr' comp $ Just stateOuts'
    where
        initVarsExpr' = initVarsExpr (map (first unwrapBnd . fromVarReceive) vars) []
        stateOuts' =
            foldr (Stmt . SendData) (Lit UnitLit) stateOuts

genCtrl :: FunCtrl embExpr annot ty -> TaskExpr embExpr annot ty
genCtrl (FunCtrl ctrlInput vars) =
    EndlessLoop $
        genCtrl' ctrlInput initVarsExpr' sendCode Nothing
    where
        initVarsExpr' = initVarsExpr receiveVars []
        receiveVars = toList $ NE.map (first (("var_" <>) . show) . second snd) $ NE.zip [0..] vars
        sendCode =
            foldr (\(bnd, OutputChannel ch) c -> Stmt (SendData $ SSend ch $ Left bnd) c) (Lit UnitLit) sendVars
        sendVars = NE.map (bimap (("var_" <>) . show) fst) $ NE.zip [0..] vars

genLitCtrl :: (Show ty, Show embExpr) => FusedLitCtrl embExpr annot ty -> TaskExpr embExpr annot ty
genLitCtrl (FusedLitCtrl ctrlInput (OutputChannel (SChan output), input) comp) =
    EndlessLoop $
        genCtrl' ctrlInput (Let output (Lit input)) (genComp comp) Nothing
    where
      genComp (Left c) = genFused' c
      genComp (Right fun) =
        F.genFun $
        case fun of
            (F.PureFusable args app res) -> F.PureFusable (map f args) app res
            -- by definition of findLonelyLiterals in ohua-core
            (F.STFusable _stateArg _args app _res _stateRes) -> error $ "Invariant broken: only pure functions need to be contextified! > contextified function: " <> show app
            (F.IdFusable arg res) -> F.IdFusable (f arg) res

      f (F.Arg (SRecv _ (SChan ch))) = F.Converted $ Var ch
      f (F.Drop (Left (SRecv _ (SChan ch)))) = F.Drop $ Right $ Var ch
      f e  = e


genCtrl' ::  CtrlInput embExpr annot ty -> (TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty) -> TaskExpr embExpr annot ty -> Maybe (TaskExpr embExpr annot ty) -> TaskExpr embExpr annot ty
genCtrl' ctrlInput initVars comp cont =
    sigStateInit "renew" $
    initVars $
    stmt $
    While (Not $ Var "renew") $
        sigStateRecv ctrlInput $
        Stmt
            (ctxtLoop
                comp) $
            sigStateRenew "renew"
    where
        stmt loop = maybe loop (Stmt loop) cont


-- assumption: ctrl before merging and as such there is only a single input
-- the below two functions actually belong together: dependent types needed!?

-- | A context control marks the end of a fusion chain. It is the very last control
--   and therefore can only be fused into.
mkLittedCtrl :: Com 'Recv embExpr annot ty -> Lit embExpr annot ty Resolved -> Com 'Channel embExpr annot ty -> LitCtrl embExpr annot ty
mkLittedCtrl ctrl lit out =
    LitCtrl (CtrlInput ctrl) (OutputChannel out, lit)

mkCtrl :: Com 'Recv embExpr annot ty -> Com 'Recv embExpr annot ty -> Com 'Channel embExpr annot ty -> VarCtrl embExpr annot ty
mkCtrl ctrlInput input output =
    VarCtrl (CtrlInput ctrlInput) (OutputChannel output, input)

sigStateInit :: Binding -> TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty
sigStateInit bnd = Let bnd (Lit $ BoolLit False)

sigStateRecv ::  CtrlInput embExpr annot ty -> TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty
sigStateRecv (CtrlInput ctrlInput) cont =
    Let "sig" (ReceiveData ctrlInput) $
    Let "count" (L.secondIndexing "sig") cont

ctxtLoop :: TaskExpr embExpr annot ty -> TaskExpr embExpr annot ty
ctxtLoop = Repeat $ Left "count"

sigStateRenew :: Binding -> TaskExpr embExpr annot ty
sigStateRenew bnd =
    Let "renew_next_time" (L.firstIndexing "sig") $
    Assign bnd $ Var "renew_next_time"
