module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)
import Ohua.Backend.Operators.State
import Ohua.Backend.Operators.Function as F

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import qualified Text.Show -- for implementing the Show instance

type CtrlInput = Binding
type OutputChannel = Binding

-- FIXME remove the functions from the data type.
data Ctrl f where
    Ctrl :: (Foldable f)
        => (Binding -> TaskExpr -> TaskExpr) -- signal state init
        -> CtrlInput
        -> f (OutputChannel, Recv) -- var(s)
        -- below here is computation code
        -> (Binding -> TaskExpr -> TaskExpr) -- signal state receive code
        -> (TaskExpr -> TaskExpr) -- ctxt loop
        -> (Binding -> TaskExpr) -- signal state renewal
        -> Ctrl f

data LittedCtrl = LittedCtrl CtrlInput Lit Binding deriving (Eq, Show, Generic)

instance Hashable LittedCtrl

instance Eq (Ctrl f) where
    (Ctrl _ cInp insOuts _ _ _) == (Ctrl _ cInp' insOuts' _ _ _) = 
        cInp == cInp' && concatMap (:[]) insOuts == concatMap (:[]) insOuts'

instance Hashable (Ctrl f) where
    hashWithSalt s (Ctrl _ cInp insOuts _ _ _) = 
        s `hashWithSalt` cInp `hashWithSalt` concatMap (:[]) insOuts

instance Show (Ctrl f) where
    show (Ctrl _ cInp insOuts _ _ _) = 
        "Ctrl { ctrlInput=" <> show cInp <> 
        ", insOuts: " <> show (concatMap (:[]) insOuts) <> 
        "}"

type VarCtrl = Ctrl Identity
type FunCtrl = Ctrl NonEmpty -- FIXME this is not the right type! It does not transport the 
                             -- invariant that all output channels have the same target.
                             -- This is what `merge` establishes.
                             -- Is this again something that can be created with LiquidHaskell?

ctrlReceives :: FunCtrl -> NonEmpty Binding
ctrlReceives (Ctrl _ _ vars _ _ _) = map ((\(Recv _ b) -> b) . snd) vars

toFunCtrl :: VarCtrl -> FunCtrl
toFunCtrl (Ctrl sigStateInit ctrlVar (Identity var) sigStateRecv ctxtLoop sigStateRenewal) = 
    Ctrl sigStateInit ctrlVar (var:|[]) sigStateRecv ctxtLoop sigStateRenewal

data VarReceive 
    = StateVar OutputChannel Recv 
    | PureVar OutputChannel Recv
    deriving (Eq, Show, Generic)

instance Hashable VarReceive

from :: VarReceive -> (OutputChannel, Recv)
from (StateVar c r) = (c,r)
from (PureVar c r) = (c,r)

-- FIXME remove the functions from the data type!
data FusedCtrl
    = FusedCtrl
        (Binding -> TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty VarReceive) -- vars
        -- below here is computation code
        (Binding -> TaskExpr -> TaskExpr) -- signal state receive code
        TaskExpr -- comp
        (Binding -> TaskExpr) -- signal state renewal
        [Send] -- state outs

-- FIXME I'm sure this will collapse with the above data type once the functions were removed.
data FusedLittedCtrl
    = FusedLittedCtrl
        CtrlInput
        Lit
        OutputChannel
        TaskExpr
    deriving (Eq, Show, Generic)

instance Hashable FusedLittedCtrl

instance Show FusedCtrl where
    show (FusedCtrl _ ins _ comp _ outs) = 
        "FusedCtrl { ins: " <> show ins <> ", comp: " <> show comp <> ", outs: " <> show outs <> "}"

instance Eq FusedCtrl where
    (FusedCtrl _ ins _ comp _ outs) == (FusedCtrl _ ins' _ comp' _ outs') =
        ins == ins' && comp == comp' && outs == outs'

instance Hashable FusedCtrl where
    hashWithSalt s (FusedCtrl _ ins _ comp _ outs) =
        s `hashWithSalt` ins `hashWithSalt` comp `hashWithSalt` outs

fuseSTCSMap :: STCLangSMap -> FusedCtrl -> FusedCtrl
fuseSTCSMap
    (STCLangSMap init ctxtLoop stateReceive stateOut)
    (FusedCtrl sigStateInit vars sigStateRecv comp sigStateRenewal stateOuts)
    = FusedCtrl
        sigStateInit
        vars
        sigStateRecv
        comp
        sigStateRenewal
        stateOuts'
    where
        stateOuts' = 
            map 
                (\(Emit ch d) -> if ch == stateReceive
                                    then Emit stateOut d
                                    else Emit ch d) 
                stateOuts

fuseCtrl :: FunCtrl -> FusedCtrl -> FusedCtrl
fuseCtrl 
    (Ctrl sigStateInit _ vars sigStateRecv ctxtLoop sigStateRenewal) -- from
    (FusedCtrl sigStateInit' vars' sigStateRecv' comp' sigStateRenewal' stateOuts) -- to
    = FusedCtrl
        sigStateInit
        vars''
        sigStateRecv
        -- TODO refactor and use genFused!
        (ctxtLoop $
            sigStateInit' "renew" $
            initVarsExpr' $
            While (Not $ Var "renew") $
                sigStateRecv' "renew" $
                Stmt
                    comp' $
                    sigStateRenewal' "renew")
        sigStateRenewal
        stateOuts'
    where
        initVarsExpr' = initVarsExpr (map from vars') $ NE.toList sendVars
        sendVars = NE.map fst vars
        stateVars = 
            HS.fromList $
            catMaybes $
            NE.toList $
            NE.map (\case StateVar _outChan (Recv _ b) -> Just b; _ -> Nothing)
            vars'
        vars'' = 
            NE.map 
                (\(b,re@(Recv _ r)) -> 
                    if HS.member b stateVars
                    then StateVar b re
                    else PureVar b re) 
                vars
        stateOuts' = map g stateOuts
        g (Emit ch d) = 
            let d' = case NE.filter (\(v,r) -> v == d) vars of
                        [(v,_)] -> v
                        _ -> error "invariant broken"
            in Emit ch d' -- the var that I assign the state to becomes the new data out for the state

fuseFun :: FunCtrl -> FusableFunction -> FusedCtrl
fuseFun (Ctrl sigStateInit _ vars sigStateRecv ctxtLoop sigStateRenewal) =
    \case
        (PureFusable args app res) -> 
            FusedCtrl
                sigStateInit
                (NE.map (uncurry PureVar) vars)
                sigStateRecv
                ( ctxtLoop $
                    F.genFun $ 
                        PureFusable (map f args) app res
                )
                sigStateRenewal
                []
        (STFusable stateArg args app res stateRes) ->
            FusedCtrl
                sigStateInit
                (vars' stateArg)
                sigStateRecv
                ( ctxtLoop $
                    F.genFun $ 
                        STFusable stateArg (map f args) app res Nothing
                )
                sigStateRenewal $
                maybe [] (\g -> [g $ fst $ stateVar stateArg]) stateRes
        where
            f (Arg (Recv _ ch)) | HS.member ch ctrled = Converted $ Var ch
            f (Drop (Left (Recv _ ch))) | HS.member ch ctrled = Drop $ Right $ Var ch
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

fuseLittedCtrlIntoCtrl :: LittedCtrl -> FusedCtrl -> FusedLittedCtrl
fuseLittedCtrlIntoCtrl (LittedCtrl ctrlInp lit out) = FusedLittedCtrl ctrlInp lit out . genFused'

fuseLittedCtrlIntoFun :: LittedCtrl -> FusableFunction -> FusedLittedCtrl
fuseLittedCtrlIntoFun (LittedCtrl ctrlInp lit out) = 
    FusedLittedCtrl ctrlInp lit out . genFun .
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
merge
    (Ctrl sigStateInit sigSource (Identity var) sigStateRecv ctxtLoop sigStateRenewal)
    c
    = 
    let vars' = case c of
                    (Left (Ctrl _ _sigSource' (Identity var') _ _ _)) -> [var, var']
                    (Right (Ctrl _ _sigSource' vars _ _ _)) -> var NE.<| vars
    in Ctrl
        sigStateInit
        sigSource
        vars'
        sigStateRecv
        ctxtLoop
        sigStateRenewal

genFused :: FusedCtrl -> TaskExpr
genFused = EndlessLoop . genFused'

genFused' :: FusedCtrl -> TaskExpr
genFused' (FusedCtrl sigStateInit vars sigStateRecv comp sigStateRenewal stateOuts) =    
    sigStateInit "renew" $
    initVarsExpr' $
    Stmt (
        While (Not $ Var "renew") $
        sigStateRecv "renew" $
        Stmt
            comp $
            sigStateRenewal "renew"
    ) stateOuts'
    where
        initVarsExpr' = initVarsExpr (map from vars) []
        stateOuts' = 
            foldr (\(Emit ch d) c -> Stmt (Send ch d) c) (Lit UnitLit) stateOuts

genCtrl :: FunCtrl -> TaskExpr
genCtrl (Ctrl sigStateInit _ vars sigStateRecv ctxtLoop sigStateRenewal) = 
    -- refactor with genFused and genCtxtCtrl
    EndlessLoop $
        sigStateInit "renew" $
        initVarsExpr' $
        While (Not $ Var "renew") $
            sigStateRecv "renew" $
            Stmt
                (ctxtLoop
                    sendCode) $
                sigStateRenewal "renew"
    where
        initVarsExpr' = initVarsExpr receiveVars []
        receiveVars = NE.map (first (("var_" <>) . show) . second snd) $ NE.zip [0..] vars
        sendVars = NE.map (first (("var_" <>) . show) . second fst) $ NE.zip [0..] vars
        sendCode = 
            foldr (\(bnd, ch) c -> Stmt (Send ch bnd) c) (Lit UnitLit) sendVars

genLittedCtrl :: FusedLittedCtrl -> TaskExpr
genLittedCtrl (FusedLittedCtrl ctrlInput input output comp) = 
    -- TODO refactor with genCtrl and genFusedCtrl!
    EndlessLoop $
        sigStateInit "renew" $
        Let output (Lit input) $
        While (Not $ Var "renew") $
            sigStateRecv "renew" $
            Stmt
                (ctxtLoop
                    comp) $
                sigStateRenew "renew"
    where
        sigStateInit :: Binding -> TaskExpr -> TaskExpr
        sigStateInit bnd = Let bnd (Lit $ BoolLit False)

        sigStateRecv :: Binding -> TaskExpr -> TaskExpr
        sigStateRecv renew cont = 
            Let "sig" (Receive 0 ctrlInput) $
            Let "count" (L.Second "sig") cont
        
        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "count"

        sigStateRenew :: Binding -> TaskExpr
        sigStateRenew bnd = 
            Let "renew_next_time" (L.First "sig") $
            Assign bnd $ Var "renew_next_time"


initVarsExpr :: NonEmpty (Binding, Recv) -> [Binding] -> TaskExpr -> TaskExpr
initVarsExpr rVars sVars cont = 
    foldr (\(bnd, e) c -> Let bnd e c) cont $ toExpr rVars sVars

receiveVarsExpr :: NonEmpty (Binding, Recv) -> [Binding] -> TaskExpr
receiveVarsExpr rVars sVars = 
    foldr (\(bnd, e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: NonEmpty (Binding, Recv) -> [Binding] -> NonEmpty (Binding, TaskExpr)
toExpr rVars sVars  = NE.map (second f) rVars
    where
        f (Recv _ ch) | HS.member ch ctrled = Var ch
        f (Recv i ch)  = Receive i ch
        ctrled = HS.fromList sVars


-- assumption: ctrl before merging and as such there is only a single input
-- the below two functions actually belong together: dependent types needed!

-- | A context control marks the end of a fusion chain. It is the very last control
--   and therefore can only be fused into.
mkLittedCtrl :: CtrlInput -> Lit -> Binding -> LittedCtrl
mkLittedCtrl = LittedCtrl

mkCtrl :: CtrlInput -> Binding -> Binding -> VarCtrl
mkCtrl ctrlInput input output = 
    Ctrl 
        sigStateInit
        ctrlInput
        (Identity (output, Recv 0 input))
        sigStateRecv
        ctxtLoop 
        sigStateRenew
    where
        sigStateInit :: Binding -> TaskExpr -> TaskExpr
        sigStateInit bnd = Let bnd (Lit $ BoolLit False)

        sigStateRecv :: Binding -> TaskExpr -> TaskExpr
        sigStateRecv renew cont = 
            Let "sig" (Receive 0 ctrlInput) $
            Let "count" (L.Second "sig") cont
        
        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "count"

        sigStateRenew :: Binding -> TaskExpr
        sigStateRenew bnd = 
            Let "renew_next_time" (L.First "sig") $
            Assign bnd $ Var "renew_next_time"

-- -- invariants:  length Inputs == length Outputs, NonEmpty Inputs, NonEmpty Outputs
-- ctrl' :: CtrlInput -> Inputs -> Outputs -> TaskExpr
-- ctrl' ctrlInput inputs outputs = 
--     Let "renew" (Lit $ BoolLit False) $
--     stateReceiveCode Let $
--     EndlessLoop $
--         Let "sig" (Receive 0 ctrlInput) $
--         Let "renew_next_time" (L.First $ Left "sig") $
--         Let "count" (L.Second $ Left "sig") $
--         Stmt 
--         ( Cond (Var "renew")
--             (stateReceiveCode (\v r c -> Stmt (Assign v r) c) $ Lit UnitLit)
--             (Lit UnitLit) ) $
--         Stmt
--         ( Repeat (Left "count")
--             sendCode ) $
--         Assign "renew" (Var "renew_next_time")
--     where
--         stateReceiveCode f cont = 
--             foldr (\(i,input) c -> f ("state" <> show i) (Receive 0 input) c) cont $ zip [1..] inputs
--         sendCode = foldr (\(o,output) c ->  Stmt (Send output ("state" <> show o)) c) (Lit UnitLit) $ zip [1..] outputs

-- ctrlNoAssign :: CtrlInput -> Inputs -> Outputs -> (TaskExpr, NonEmpty (Function TaskExpr))
-- ctrlNoAssign ctrlInput inputs outputs = 
--     let initFun = 
--             Function (QualifiedBinding (makeThrow []) "init") [] $
--                 stateReceiveCode Let $
--                 Apply $ Stateless (QualifiedBinding (makeThrow []) "ctrl") $ map Var stateVars
--         ctrlFun =
--             Function (QualifiedBinding (makeThrow []) "ctrl") stateVars $
--                 Let "sig" (Receive 0 ctrlInput) $
--                 Let "renew_next_time" (L.First $ Left "sig") $
--                 Let "count" (L.Second $ Left "sig") $
--                 Stmt
--                 ( Repeat (Left "count")
--                     sendCode ) $
--                 Cond (Var "renew_next_time")
--                     (Apply $ Stateless (QualifiedBinding (makeThrow []) "init") [])
--                     (Apply $ Stateless (QualifiedBinding (makeThrow []) "ctrl") $ map Var stateVars)
--         expr = Apply $ Stateless (QualifiedBinding (makeThrow []) "init") []
--     in (expr, [initFun, ctrlFun])
--     where
--         stateReceiveCode f cont = 
--             foldr (\(input, i) c -> f ("state" <> show i) (Receive 0 input) c) cont $ zip inputs [1..]
--         sendCode = foldr (\(output, o) c ->  Stmt (Send output ("state" <> show o)) c) (Lit UnitLit) $ zip outputs [1..]
--         stateVars = ["state" <> show i | i <- [1..(length inputs)]]