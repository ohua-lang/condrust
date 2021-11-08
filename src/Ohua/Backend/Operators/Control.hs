{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds, DeriveGeneric, OverloadedLists #-}
module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Operators.Common
import Ohua.Backend.Lang as L hiding (Function)
import Ohua.Backend.Operators.State
import qualified Ohua.Backend.Operators.Function as F
import qualified Ohua.Backend.Operators.SMap as SMap

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS
import qualified GHC.Exts as Ext (IsList(..))

newtype  CtrlInput ty = CtrlInput (Com 'Recv ty) deriving (Eq, Show, Generic)

instance Hashable (CtrlInput ty)

-- annotations
data CtrlAnno = Variable | Fun | CtxtLit deriving (Eq, Show, Generic)

data Ctrl (anno::CtrlAnno) (ty::Type) :: Type where
    VarCtrl ::  CtrlInput ty -> (OutputChannel ty, Com 'Recv ty) -> Ctrl 'Variable ty
    -- TODO this is not the right type! It does not transport the 
    -- invariant that all output channels have the same target.
    -- This is what `merge` establishes.
    -- Is this again something that can be created with LiquidHaskell?
    FunCtrl ::  CtrlInput ty -> NonEmpty (OutputChannel ty, Com 'Recv ty) -> Ctrl 'Fun ty
    LitCtrl ::  CtrlInput ty -> (OutputChannel ty, Lit ty) -> Ctrl 'CtxtLit ty

deriving instance Eq (Ctrl semTy ty)
deriving instance Show (Ctrl semTy ty)

instance Hashable (Ctrl semTy ty) where
    hashWithSalt s (VarCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut
    hashWithSalt s (FunCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut
    hashWithSalt s (LitCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut

type VarCtrl = Ctrl 'Variable
type FunCtrl = Ctrl 'Fun
type LitCtrl = Ctrl 'CtxtLit

ctrlReceives :: FunCtrl ty -> NonEmpty (Com 'Recv ty)
ctrlReceives (FunCtrl _ vars) = map snd vars

toFunCtrl :: VarCtrl ty -> FunCtrl ty
toFunCtrl (VarCtrl ctrlVar var) = FunCtrl ctrlVar (var:|[])

data FusedCtrlAnno = Function | Literal deriving (Eq, Show, Generic)

data FusedCtrl (anno::FusedCtrlAnno) (ty::Type) :: Type where
    -- there is another assumption here that needs to be enforced:
    -- state inputs in NonEmpty VarReceive map to state outputs in [Send]!
    FusedFunCtrl ::  CtrlInput ty -> [VarReceive ty] -> TaskExpr ty -> [Com 'Send ty] -> FusedCtrl 'Function ty
    FusedLitCtrl ::  CtrlInput ty -> (OutputChannel ty, Lit ty) -> Either (FusedFunCtrl ty) (F.FusableFunction ty) -> FusedCtrl 'Literal ty

deriving instance Eq (FusedCtrl semTy ty)

type FusedFunCtrl = FusedCtrl 'Function
type FusedLitCtrl = FusedCtrl 'Literal

instance Hashable (FusedCtrl anno ty) where
    hashWithSalt s (FusedFunCtrl cInp inOut comp stateOuts) = 
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp `hashWithSalt` stateOuts
    hashWithSalt s (FusedLitCtrl cInp inOut comp) = 
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp

fuseSTCSMap :: STCLangSMap 'Fusable ty -> FusedFunCtrl ty -> FusedFunCtrl ty
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
fuseCtrl :: forall ty.FunCtrl ty -> FusedFunCtrl ty -> FusedFunCtrl ty
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
fuseCtrlIntoFun :: forall ty. F.FusableFunction ty -> FusedFunCtrl ty -> F.FusedFun ty
fuseCtrlIntoFun fun (FusedFunCtrl ctrlIn ins expr outs) =
    case fun of
        (F.PureFusable recvs qb out) ->
            forPure out (toList out) ins $ F.PureFusable recvs qb
        (F.STFusable sRecv recvs qb out sOut) ->
            let ins' = filterState sOut ins
                sOut' = if length ins' < length ins then Nothing else sOut
             in forPure out (toList out) ins' (\o ->F.STFusable (F.Arg sRecv) recvs qb o sOut')
        (F.IdFusable recv out) ->
            forPure out (toList out) [] $ F.IdFusable recv
   where
     filterState Nothing ins0 = ins0
     filterState (Just out) ins0 =
       filter ((\(SRecv _ inp) -> inp /= out) . snd . fromVarReceive) ins0

     filterData [] ins0 = ins0
     filterData outs ins0 =
       filter
       ((\(SRecv _ inp) -> not $ HS.member inp $ HS.fromList outs) .
        snd .
        fromVarReceive)
       ins0

     forPure :: ( Ext.IsList (c (Com 'Channel ty))
                , Ext.IsList (c (F.Result ty))
                , Functor c)
             => c (Com 'Channel ty)
             -> [Com 'Channel ty]
             -> [VarReceive ty]
             -> ((c (F.Result ty) -> F.FusedFunction ty) -> F.FusedFun ty)
     forPure out outsL ins' =
       -- I could not get this to work :(
       -- let ins'' = filterData (Ext.toList out) ins'
       let ins'' = filterData outsL ins'
           outHS = HS.fromList $ map ((\(SRecv _ inp) -> inp) . snd . fromVarReceive) ins
           out' = map (\o -> case HS.member o outHS of
                               True -> F.DropResult
                               False -> F.SendResult o
                      )
                  out
       in \f ->
            F.FusedFun (f out') $
            genFused $
            FusedFunCtrl ctrlIn ins'' expr outs


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
fuseFun :: FunCtrl ty -> F.FusableFunction ty -> FusedFunCtrl ty
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
                 (map F.SendResult res)
                 Nothing)
                (maybe
                    []
                    (\g -> [SSend g $ unwrapBnd $ fst $ stateVar stateArg])
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
                []  -> error "invariant broken" -- an assumption rooted inside DFLang
            vars' sArg args =
                toList $
                NE.map ((\case
                            (bnd, a) | a == sArg -> StateVar bnd a
                            (bnd, a) -> PureVar bnd a)
                        . propagateTypeFromArg args)
                        vars
            propagateTypeFromArg =
              propagateType . mapMaybe (\case (F.Arg s) -> Just s; _ -> Nothing)

fuseSMap :: FunCtrl ty -> SMap.Op ty -> FusedFunCtrl ty
fuseSMap (FunCtrl ctrlInput (( o, inData):|[])) smap = -- invariant
  FusedFunCtrl
    ctrlInput
    [PureVar o inData]
    (SMap.gen' $ SMap.fuse (unwrapBnd o) smap)
    []

propagateType :: [Com 'Recv ty] -> (OutputChannel ty, Com 'Recv ty) -> (OutputChannel ty, Com 'Recv ty)
propagateType args (o@(OutputChannel outChan), r@(SRecv _ rChan)) =
    foldl (\s out -> case out of
                        (SRecv t chan) | chan == outChan -> (o, SRecv t rChan)
                        -- TODO this is actually not possible and should be an invariant because all vars
                        --      of the control have a corresponding arg.
                        _ -> s)
        (o,r)
        args

fuseLitCtrlIntoCtrl :: LitCtrl ty ->  FusedFunCtrl ty -> FusedLitCtrl ty
fuseLitCtrlIntoCtrl (LitCtrl ctrlInp inOut) = FusedLitCtrl ctrlInp inOut . Left

fuseLitCtrlIntoFun :: LitCtrl ty -> F.FusableFunction ty -> FusedLitCtrl ty
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
merge :: VarCtrl ty -> Either (VarCtrl ty) (FunCtrl ty) -> FunCtrl ty
merge (VarCtrl ctrlIn var) (Left (VarCtrl _ctrlIn' var')) = FunCtrl ctrlIn [var, var']
merge (VarCtrl ctrlIn var) (Right (FunCtrl _ctrlIn' vars)) = FunCtrl ctrlIn $ var NE.<| vars

genFused ::  FusedFunCtrl ty -> TaskExpr ty
genFused = EndlessLoop . genFused'

genFused' ::  FusedFunCtrl ty -> TaskExpr ty
genFused' (FusedFunCtrl ctrlInput vars comp stateOuts) =
    genCtrl' ctrlInput initVarsExpr' comp $ Just stateOuts'
    where
        initVarsExpr' = initVarsExpr (map (first unwrapBnd . fromVarReceive) vars) []
        stateOuts' =
            foldr (Stmt . SendData) (Lit UnitLit) stateOuts

genCtrl :: FunCtrl ty -> TaskExpr ty
genCtrl (FunCtrl ctrlInput vars) =
    EndlessLoop $
        genCtrl' ctrlInput initVarsExpr' sendCode Nothing
    where
        initVarsExpr' = initVarsExpr receiveVars []
        receiveVars = toList $ NE.map (first (("var_" <>) . show) . second snd) $ NE.zip [0..] vars
        sendCode =
            foldr (\(bnd, OutputChannel ch) c -> Stmt (SendData $ SSend ch bnd) c) (Lit UnitLit) sendVars
        sendVars = NE.map (first (("var_" <>) . show) . second fst) $ NE.zip [0..] vars

genLitCtrl :: FusedLitCtrl ty -> TaskExpr ty
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


genCtrl' ::  CtrlInput ty -> (TaskExpr ty -> TaskExpr ty) -> TaskExpr ty -> Maybe (TaskExpr ty) -> TaskExpr ty
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
mkLittedCtrl :: Com 'Recv ty -> Lit ty -> Com 'Channel ty -> LitCtrl ty
mkLittedCtrl ctrl lit out =
    LitCtrl (CtrlInput ctrl) (OutputChannel out, lit)

mkCtrl :: Com 'Recv ty -> Com 'Recv ty -> Com 'Channel ty -> VarCtrl ty
mkCtrl ctrlInput input output =
    VarCtrl (CtrlInput ctrlInput) (OutputChannel output, input)

sigStateInit :: Binding -> TaskExpr ty -> TaskExpr ty
sigStateInit bnd = Let bnd (Lit $ BoolLit False)

sigStateRecv ::  CtrlInput ty -> TaskExpr ty -> TaskExpr ty
sigStateRecv (CtrlInput ctrlInput) cont = 
    Let "sig" (ReceiveData ctrlInput) $
    Let "count" (L.Second "sig") cont

ctxtLoop :: TaskExpr ty -> TaskExpr ty
ctxtLoop = Repeat $ Left "count"

sigStateRenew :: Binding -> TaskExpr ty
sigStateRenew bnd = 
    Let "renew_next_time" (L.First "sig") $
    Assign bnd $ Var "renew_next_time"
