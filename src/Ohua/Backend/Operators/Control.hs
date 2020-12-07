{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds, DeriveGeneric #-}
module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)
import Ohua.Backend.Operators.State
import Ohua.Backend.Operators.Function as F

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS


newtype  CtrlInput ty = CtrlInput (Com 'Recv ty) deriving (Eq, Generic)
newtype OutputChannel ty = OutputChannel (Com 'Channel ty) deriving (Eq, Generic)

instance Hashable (CtrlInput ty)
instance Hashable (OutputChannel ty)

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

-- FIXME Should use annotations ST and Data from BindingType
data VarReceive ty
    = StateVar (OutputChannel ty) (Com 'Recv ty)
    | PureVar (OutputChannel ty) (Com 'Recv ty)
    deriving (Eq,Generic)

instance Hashable (VarReceive ty)

fromVarReceive :: VarReceive ty -> (OutputChannel ty, Com 'Recv ty)
fromVarReceive (StateVar c r) = (c,r)
fromVarReceive (PureVar c r) = (c,r)

unwrapChan :: OutputChannel ty -> Com 'Channel ty
unwrapChan (OutputChannel c) = c

unwrapBnd :: OutputChannel ty -> Binding 
unwrapBnd (OutputChannel (SChan b)) = b

data FusedCtrlAnno = Function | Literal deriving (Eq, Show, Generic)

data FusedCtrl (anno::FusedCtrlAnno) (ty::Type) :: Type where
    -- there is another assumption here that needs to be enforced: 
    -- state inputs in NonEmpty VarReceive map to state outputs in [Send]!
    FusedFunCtrl ::  CtrlInput ty -> NonEmpty (VarReceive ty) -> TaskExpr ty -> [Com 'Send ty] -> FusedCtrl 'Function ty
    FusedLitCtrl ::  CtrlInput ty -> (OutputChannel ty, Lit ty) -> TaskExpr ty -> FusedCtrl 'Literal ty

deriving instance Eq (FusedCtrl semTy ty)

type FusedFunCtrl = FusedCtrl 'Function
type FusedLitCtrl = FusedCtrl 'Literal

instance Hashable (FusedCtrl anno ty) where
    hashWithSalt s (FusedFunCtrl cInp inOut comp stateOuts) = 
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp `hashWithSalt` stateOuts
    hashWithSalt s (FusedLitCtrl cInp inOut comp) = 
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp

fuseSTCSMap :: STCLangSMap ty -> FusedFunCtrl ty -> FusedFunCtrl ty
fuseSTCSMap
    (STCLangSMap _init _ctxtLoop (SRecv _ stateReceive) stateOut)
    (FusedFunCtrl ctrlInput vars comp stateOuts)
    = FusedFunCtrl ctrlInput vars comp stateOuts'
    where
        stateOuts' = map (\(SSend ch d) -> if ch == stateReceive
                                            then SSend stateOut d
                                            else SSend ch d) 
                         stateOuts

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
            catMaybes $
            NE.toList $
            NE.map (\case StateVar _outChan (SRecv _ b) -> Just b; _ -> Nothing)
            vars'
        vars'' = 
            NE.map 
                ((\(out@(OutputChannel b), re) -> 
                    if HS.member b stateVars
                    then StateVar out re
                    else PureVar out re)
                . propagateTypeFromRecv vars)
                vars
        stateOuts' = map g stateOuts
        g (SSend ch d) = 
            let d' = case NE.filter (\(OutputChannel (SChan o),_r) -> o == d) vars of
                        [(OutputChannel (SChan o),_)] -> o
                        _ -> error "invariant broken"
            in SSend ch d' -- the var that I assign the state to becomes the new data out for the state
        propagateTypeFromRecv = propagateType . toList . map snd

fuseFun :: FunCtrl ty -> FusableFunction ty -> FusedFunCtrl ty
fuseFun (FunCtrl ctrlInput vars) =
    \case
        (PureFusable args app res) -> 
            FusedFunCtrl
                ctrlInput
                (NE.map (uncurry PureVar . propagateTypeFromArg args) vars)
                (F.genFun' $  PureFusable (map f args) app res)
                []
        (STFusable stateArg args app res stateRes) ->
            FusedFunCtrl
                ctrlInput
                (vars' stateArg args)
                (F.genFun' $ STFusable stateArg (map f args) app res Nothing)
                (maybe 
                    [] 
                    (\g -> [SSend g $ unwrapBnd $ fst $ stateVar stateArg]) 
                    stateRes)
        where
            f (Arg (SRecv _ (SChan ch))) | HS.member (SChan ch) ctrled = Converted $ Var ch
            f (Drop (Left (SRecv _ (SChan ch)))) | HS.member (SChan ch) ctrled = Drop $ Right $ Var ch
            f e  = e
            ctrled = HS.fromList $ toList $ map unwrapChan sendVars
            sendVars = NE.map fst vars
            stateVar sArg = case NE.filter (\(_, r) -> r == sArg) vars of
                                [s] -> s
                                [] -> error "invariant broken" -- an assumption rooted inside DFLang
            vars' sArg args = 
                NE.map ((\case
                            (bnd, a) | a == sArg -> StateVar bnd a
                            (bnd, a) -> PureVar bnd a)
                        . propagateTypeFromArg args)
                        vars
            propagateTypeFromArg = propagateType . mapMaybe (\case (Arg s) -> Just s; _ -> Nothing)

propagateType :: [Com 'Recv ty] -> (OutputChannel ty, Com 'Recv ty) -> (OutputChannel ty, Com 'Recv ty)
propagateType args (o@(OutputChannel outChan), r@(SRecv _ rChan)) = 
    foldl (\s out -> case out of 
                        (SRecv t chan) | chan == outChan -> (o, SRecv t rChan)
                        -- TODO this is actually not possible and should be an invariant because all vars
                        --      of the control of a corresponding arg.
                        _ -> s)
        (o,r)
        args

fuseLitCtrlIntoCtrl :: LitCtrl ty ->  FusedFunCtrl ty -> FusedLitCtrl ty
fuseLitCtrlIntoCtrl (LitCtrl ctrlInp inOut) = FusedLitCtrl ctrlInp inOut . genFused'

fuseLitCtrlIntoFun :: LitCtrl ty -> FusableFunction ty -> FusedLitCtrl ty
fuseLitCtrlIntoFun (LitCtrl ctrlInp outIn) = 
    FusedLitCtrl ctrlInp outIn . genFun .
        (\case 
            (PureFusable args app res) -> PureFusable (map f args) app res
            (STFusable _stateArg _args _app _res _stateRes) -> error "Invariant broken: only pure functions need to be contextified!")
    where
        f (Arg (SRecv _ (SChan ch))) = Converted $ Var ch
        f (Drop (Left (SRecv _ (SChan ch)))) = Drop $ Right $ Var ch
        f e  = e

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
        receiveVars = NE.map (first (("var_" <>) . show) . second snd) $ NE.zip [0..] vars
        sendCode = 
            foldr (\(bnd, OutputChannel ch) c -> Stmt (SendData $ SSend ch bnd) c) (Lit UnitLit) sendVars
        sendVars = NE.map (first (("var_" <>) . show) . second fst) $ NE.zip [0..] vars

genLitCtrl :: FusedLitCtrl ty -> TaskExpr ty
genLitCtrl (FusedLitCtrl ctrlInput (OutputChannel (SChan output), input) comp) = 
    EndlessLoop $ 
        genCtrl' ctrlInput (Let output (Lit input)) comp Nothing

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

initVarsExpr :: NonEmpty (Binding, Com 'Recv ty) -> [OutputChannel ty] -> TaskExpr ty -> TaskExpr ty
initVarsExpr rVars sVars cont = 
    foldr (\(bnd, e) c -> Let bnd e c) cont $ toExpr rVars sVars

receiveVarsExpr :: NonEmpty (OutputChannel ty, Com 'Recv ty) -> [OutputChannel ty] -> TaskExpr ty
receiveVarsExpr rVars sVars = 
    foldr (\(OutputChannel (SChan bnd), e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: NonEmpty (a, Com 'Recv ty) -> [OutputChannel ty] -> NonEmpty (a, TaskExpr ty)
toExpr rVars sVars  = NE.map (second f) rVars
    where
        f (SRecv _ ch@(SChan bnd)) | HS.member ch ctrled = Var bnd
        f r  = ReceiveData r
        ctrled = HS.fromList $ map unwrapChan sVars


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
