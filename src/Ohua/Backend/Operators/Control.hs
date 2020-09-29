{-# LANGUAGE EmptyDataDeriving #-}
module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)
import Ohua.Backend.Operators.State
import Ohua.Backend.Operators.Function as F

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import qualified Text.Show -- for implementing the Show instance


newtype CtrlInput = CtrlInput Binding deriving (Eq, Show, Generic)
newtype OutputChannel = OutputChannel Binding deriving (Eq, Show, Generic)

instance Hashable CtrlInput
instance Hashable OutputChannel

-- annotations
data Var deriving (Eq, Show, Generic)
data Fun deriving (Eq, Show, Generic)
data CtxtLit deriving (Eq, Show, Generic)

data Ctrl anno where
    VarCtrl :: CtrlInput -> (OutputChannel, Recv) -> Ctrl Var
    -- TODO this is not the right type! It does not transport the 
    -- invariant that all output channels have the same target.
    -- This is what `merge` establishes.
    -- Is this again something that can be created with LiquidHaskell?
    FunCtrl :: CtrlInput -> NonEmpty (OutputChannel, Recv) -> Ctrl Fun
    LitCtrl :: CtrlInput -> (OutputChannel, Lit) -> Ctrl CtxtLit

type VarCtrl = Ctrl Var
type FunCtrl = Ctrl Fun
type LitCtrl = Ctrl CtxtLit

instance Show (Ctrl anno) where
    show (VarCtrl cInp inOut) = "VarCtrl {" <>  show cInp <> "," <> show inOut <> "}"
    show (FunCtrl cInp inOut) = "FunCtrl {" <>  show cInp <> "," <> show inOut <> "}"
    show (LitCtrl cInp inOut) = "LitCtrl {" <>  show cInp <> "," <> show inOut <> "}"

instance Eq (Ctrl anno) where
    (VarCtrl cInp inOut) == (VarCtrl cInp' inOut') = cInp == cInp' && inOut == inOut'
    (FunCtrl cInp inOut) == (FunCtrl cInp' inOut') = cInp == cInp' && inOut == inOut'
    (LitCtrl cInp inOut) == (LitCtrl cInp' inOut') = cInp == cInp' && inOut == inOut'

instance Hashable (Ctrl anno) where
    hashWithSalt s (VarCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut
    hashWithSalt s (FunCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut
    hashWithSalt s (LitCtrl cInp inOut) = s `hashWithSalt` cInp `hashWithSalt` inOut

ctrlReceives :: FunCtrl -> NonEmpty Binding
ctrlReceives (FunCtrl _ vars) = map ((\(Recv _ b) -> b) . snd) vars

toFunCtrl :: VarCtrl -> FunCtrl
toFunCtrl (VarCtrl ctrlVar var) = FunCtrl ctrlVar (var:|[])

data VarReceive 
    = StateVar OutputChannel Recv 
    | PureVar OutputChannel Recv
    deriving (Eq, Show, Generic)

instance Hashable VarReceive

fromVarReceive :: VarReceive -> (OutputChannel, Recv)
fromVarReceive (StateVar c r) = (c,r)
fromVarReceive (PureVar c r) = (c,r)

unwrapBnd :: OutputChannel -> Binding
unwrapBnd (OutputChannel b) = b

data FusedCtrl anno where
    -- there is another assumption here that needs to be enforced: 
    -- state inputs in NonEmpty VarReceive map to state outputs in [Send]!
    FusedFunCtrl :: CtrlInput -> NonEmpty VarReceive -> TaskExpr -> [Send] -> FusedCtrl Fun
    FusedLitCtrl :: CtrlInput -> (OutputChannel, Lit) -> TaskExpr -> FusedCtrl Lit

type FusedFunCtrl = FusedCtrl Fun
type FusedLitCtrl = FusedCtrl Lit

instance Show (FusedCtrl anno) where
    show (FusedFunCtrl cInp inOut comp stateOuts) = 
        "FusedFunCtrl {" <> show cInp <> ", " <> show inOut <> ", "<> show comp <> ", " <> show stateOuts <> "}"
    show (FusedLitCtrl cInp inOut comp) = 
        "FusedFunCtrl {" <> show cInp <> ", " <> show inOut <> ", "<> show comp <> "}"

instance Eq (FusedCtrl anno) where
    (FusedFunCtrl cInp inOut comp stateOuts) == (FusedFunCtrl cInp' inOut' comp' stateOuts') = 
        cInp == cInp' && inOut == inOut' && comp == comp' && stateOuts == stateOuts'
    (FusedLitCtrl cInp inOut comp) == (FusedLitCtrl cInp' inOut' comp') = 
        cInp == cInp' && inOut == inOut' && comp == comp'

instance Hashable (FusedCtrl anno) where
    hashWithSalt s (FusedFunCtrl cInp inOut comp stateOuts) = 
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp `hashWithSalt` stateOuts
    hashWithSalt s (FusedLitCtrl cInp inOut comp) = 
        s `hashWithSalt` cInp `hashWithSalt` inOut `hashWithSalt` comp

fuseSTCSMap :: STCLangSMap -> FusedFunCtrl -> FusedFunCtrl
fuseSTCSMap
    (STCLangSMap init ctxtLoop stateReceive stateOut)
    (FusedFunCtrl ctrlInput vars comp stateOuts)
    = FusedFunCtrl ctrlInput vars comp stateOuts'
    where
        stateOuts' = map (\(Emit ch d) -> if ch == stateReceive
                                            then Emit stateOut d
                                            else Emit ch d) 
                         stateOuts

fuseCtrl :: FunCtrl -> FusedFunCtrl -> FusedFunCtrl
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
            NE.map (\case StateVar _outChan (Recv _ b) -> Just b; _ -> Nothing)
            vars'
        vars'' = 
            NE.map 
                (\(out@(OutputChannel b), re@(Recv _ r)) -> 
                    if HS.member b stateVars
                    then StateVar out re
                    else PureVar out re) 
                vars
        stateOuts' = map g stateOuts
        g (Emit ch d) = 
            let d' = case NE.filter (\(OutputChannel o,_r) -> o == d) vars of
                        [(OutputChannel o,_)] -> o
                        _ -> error "invariant broken"
            in Emit ch d' -- the var that I assign the state to becomes the new data out for the state

fuseFun :: FunCtrl -> FusableFunction -> FusedFunCtrl
fuseFun (FunCtrl ctrlInput vars) =
    \case
        (PureFusable args app res) -> 
            FusedFunCtrl
                ctrlInput
                (NE.map (uncurry PureVar) vars)
                (F.genFun $  PureFusable (map f args) app res)
                []
        (STFusable stateArg args app res stateRes) ->
            FusedFunCtrl
                ctrlInput
                (vars' stateArg)
                (F.genFun $ STFusable stateArg (map f args) app res Nothing)
                (maybe 
                    [] 
                    (\g -> [g $ unwrapBnd $ fst $ stateVar stateArg]) 
                    stateRes)
        where
            f (Arg (Recv _ ch)) | HS.member (OutputChannel ch) ctrled = Converted $ Var ch
            f (Drop (Left (Recv _ ch))) | HS.member (OutputChannel ch) ctrled = Drop $ Right $ Var ch
            f e  = e
            ctrled = HS.fromList $ NE.toList sendVars
            sendVars = NE.map fst vars
            stateVar sArg = case NE.filter (\(_, r) -> r == sArg) vars of
                                [s] -> s
                                [] -> error "invariant broken" -- an assumption rooted inside DFLang
            vars' sArg = 
                NE.map (\case
                            (bnd, a) | a == sArg -> StateVar bnd a
                            (bnd, a) -> PureVar bnd a)
                        vars

fuseLitCtrlIntoCtrl :: LitCtrl -> FusedFunCtrl -> FusedLitCtrl
fuseLitCtrlIntoCtrl (LitCtrl ctrlInp inOut) = FusedLitCtrl ctrlInp inOut . genFused'

fuseLitCtrlIntoFun :: LitCtrl -> FusableFunction -> FusedLitCtrl
fuseLitCtrlIntoFun (LitCtrl ctrlInp outIn) = 
    FusedLitCtrl ctrlInp outIn . genFun .
        (\case 
            (PureFusable args app res) -> PureFusable (map f args) app res
            (STFusable stateArg args app res stateRes) -> error "Invariant broken: only pure functions need to be contextified!")
    where
        f (Arg (Recv _ ch)) = Converted $ Var ch
        f (Drop (Left (Recv _ ch))) = Drop $ Right $ Var ch
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
merge :: VarCtrl -> Either VarCtrl FunCtrl -> FunCtrl
merge (VarCtrl ctrlIn var) (Left (VarCtrl _ctrlIn' var')) = FunCtrl ctrlIn [var, var']
merge (VarCtrl ctrlIn var) (Right (FunCtrl _ctrlIn' vars)) = FunCtrl ctrlIn $ var NE.<| vars

genFused :: FusedFunCtrl -> TaskExpr
genFused = EndlessLoop . genFused'

genFused' :: FusedFunCtrl -> TaskExpr
genFused' (FusedFunCtrl ctrlInput vars comp stateOuts) = 
    genCtrl' ctrlInput initVarsExpr' comp $ Just stateOuts'
    where
        initVarsExpr' = initVarsExpr (map (first unwrapBnd . fromVarReceive) vars) []
        stateOuts' = 
            foldr (\(Emit ch d) c -> Stmt (Send ch d) c) (Lit UnitLit) stateOuts

genCtrl :: FunCtrl -> TaskExpr
genCtrl (FunCtrl ctrlInput vars) = 
    EndlessLoop $
        genCtrl' ctrlInput initVarsExpr' sendCode Nothing
    where
        initVarsExpr' = initVarsExpr receiveVars []
        receiveVars = NE.map (first (("var_" <>) . show) . second snd) $ NE.zip [0..] vars
        sendCode = 
            foldr (\(bnd, OutputChannel ch) c -> Stmt (Send ch bnd) c) (Lit UnitLit) sendVars
        sendVars = NE.map (first (("var_" <>) . show) . second fst) $ NE.zip [0..] vars

genLitCtrl :: FusedLitCtrl -> TaskExpr
genLitCtrl (FusedLitCtrl ctrlInput (OutputChannel output, input) comp) = 
    EndlessLoop $ 
        genCtrl' ctrlInput (Let output (Lit input)) comp Nothing

genCtrl' :: CtrlInput -> (TaskExpr -> TaskExpr) -> TaskExpr -> Maybe TaskExpr -> TaskExpr
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

initVarsExpr :: NonEmpty (Binding, Recv) -> [OutputChannel] -> TaskExpr -> TaskExpr
initVarsExpr rVars sVars cont = 
    foldr (\(bnd, e) c -> Let bnd e c) cont $ toExpr rVars sVars

receiveVarsExpr :: NonEmpty (OutputChannel, Recv) -> [OutputChannel] -> TaskExpr
receiveVarsExpr rVars sVars = 
    foldr (\(OutputChannel bnd, e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: NonEmpty (a, Recv) -> [OutputChannel] -> NonEmpty (a, TaskExpr)
toExpr rVars sVars  = NE.map (second f) rVars
    where
        f (Recv _ ch) | HS.member (OutputChannel ch) ctrled = Var ch
        f (Recv i ch)  = Receive i ch
        ctrled = HS.fromList sVars


-- assumption: ctrl before merging and as such there is only a single input
-- the below two functions actually belong together: dependent types needed!?

-- | A context control marks the end of a fusion chain. It is the very last control
--   and therefore can only be fused into.
mkLittedCtrl :: Binding -> Lit -> Binding -> LitCtrl
mkLittedCtrl ctrl lit out = LitCtrl (CtrlInput ctrl) (OutputChannel out, lit)

mkCtrl :: Binding -> Binding -> Binding -> VarCtrl
mkCtrl ctrlInput input output = VarCtrl (CtrlInput ctrlInput) (OutputChannel output, Recv 0 input)

sigStateInit :: Binding -> TaskExpr -> TaskExpr
sigStateInit bnd = Let bnd (Lit $ BoolLit False)

sigStateRecv :: CtrlInput -> TaskExpr -> TaskExpr
sigStateRecv (CtrlInput ctrlInput) cont = 
    Let "sig" (Receive 0 ctrlInput) $
    Let "count" (L.Second "sig") cont

ctxtLoop :: TaskExpr -> TaskExpr
ctxtLoop = Repeat $ Left "count"

sigStateRenew :: Binding -> TaskExpr
sigStateRenew bnd = 
    Let "renew_next_time" (L.First "sig") $
    Assign bnd $ Var "renew_next_time"
