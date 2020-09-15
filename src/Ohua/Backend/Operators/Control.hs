module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)
import Ohua.Backend.Operators.State
import Ohua.Backend.Operators.Function as F

import qualified Data.List.NonEmpty as NE
import qualified Data.HashSet as HS


type CtrlInput = Binding
type Inputs = NonEmpty Recv
type Outputs = NonEmpty Binding

-- assumption: length (receive vars) == length (send vars)
-- FIXME: the above assumption can easily be encoded via NonEmpty (receive var, send var)
data Ctrl 
    = Ctrl
        (Binding -> TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty (Binding, Recv)) -- vars
        -- below here is computation code
        (Binding -> TaskExpr -> TaskExpr -> TaskExpr) -- signal state receive code
        (TaskExpr -> TaskExpr) -- ctxt loop
        (Binding -> TaskExpr) -- signal state renewal

data FusedCtrl
    = FusedCtrl
        (Binding -> TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty (Binding, Recv)) -- vars
        -- below here is computation code
        (Binding -> TaskExpr -> TaskExpr -> TaskExpr) -- signal state receive code
        -- (TaskExpr -> TaskExpr) -- ctxt loop
        TaskExpr -- comp
        (Binding -> TaskExpr) -- signal state renewal

fuse :: Ctrl -> FusedCtrl -> FusedCtrl
fuse 
    (Ctrl sigStateInit vars sigStateRecv ctxtLoop sigStateRenewal) -- from
    (FusedCtrl sigStateInit' receiveVars' sigStateRecv' comp' sigStateRenewal') -- to
    = FusedCtrl
        sigStateInit
        vars
        sigStateRecv
        (ctxtLoop $
            sigStateInit' "renew" $
            initVarsExpr' $
            While (Not $ Var "renew") $
                sigStateRecv' "renew" receiveVarsExpr' $
                Stmt
                    comp' $
                    sigStateRenewal' "renew")
        sigStateRenewal
    where
        initVarsExpr' = initVarsExpr receiveVars' sendVars
        receiveVarsExpr' = receiveVarsExpr receiveVars' sendVars
        sendVars = NE.map fst vars

fuseFun :: Ctrl -> FusableFunction -> FusedCtrl
fuseFun (Ctrl sigStateInit vars sigStateRecv ctxtLoop sigStateRenewal) =
    \case
        (PureFusable args app res) -> 
            FusedCtrl
                sigStateInit
                vars
                sigStateRecv
                ( ctxtLoop $
                    F.gen $ 
                    PureFusable (map f args) app res
                )
                sigStateRenewal
        (STFusable args app stateRes res) ->
            FusedCtrl
                sigStateInit
                vars
                sigStateRecv
                ( ctxtLoop $
                    F.gen $ 
                        STFusable (NE.map f args) app stateRes res
                )
                sigStateRenewal
        where
            f (Left (Recv _ ch)) | HS.member ch ctrled = Right $ Var ch
            f e  = e
            ctrled = HS.fromList $ NE.toList sendVars
            sendVars = NE.map fst vars

-- | Assumption: both controls share the same signal source and the same target:
--                             x ---> ctrl1 -------+ 
--                                      ^          |
--                                      |          v
--                    sig-source -------+        f x y
--                                      |            ^
--                                      v            |
--                             y ---> ctrl2 ---------+ 
-- That really just means that I'm removing all signaling code because it is already there.
merge :: Ctrl -> Ctrl -> Ctrl
merge     
    (Ctrl sigStateInit vars sigStateRecv ctxtLoop sigStateRenewal)
    (Ctrl _ vars' _ _ _)
    = Ctrl
        sigStateInit
        (vars <> vars')
        sigStateRecv
        ctxtLoop
        sigStateRenewal

gen :: Ctrl -> TaskExpr
gen (Ctrl sigStateInit vars sigStateRecv ctxtLoop sigStateRenewal) = 
    sigStateInit "renew" $
    initVarsExpr' $
    EndlessLoop $
        sigStateRecv "renew" receiveVarsExpr' $
        Stmt
            (ctxtLoop
                sendCode) $
            sigStateRenewal "renew"
    where
        initVarsExpr' = initVarsExpr receiveVars []
        receiveVarsExpr' = receiveVarsExpr receiveVars []
        receiveVars = NE.map (first (("var_" <>) . show) . second snd) $ NE.zip [0..] vars
        sendVars = NE.map (first (("var_" <>) . show) . second fst) $ NE.zip [0..] vars
        sendCode = 
            foldr (\(bnd, ch) c -> Stmt (Send ch bnd) c) (Lit UnitLit) sendVars

initVarsExpr :: NonEmpty (Binding, Recv) -> NonEmpty Binding -> TaskExpr -> TaskExpr
initVarsExpr rVars sVars cont = 
    foldr (\(bnd, e) c -> Let bnd e c) (Lit UnitLit) $ toExpr rVars sVars

receiveVarsExpr :: NonEmpty (Binding, Recv) -> NonEmpty Binding -> TaskExpr
receiveVarsExpr rVars sVars = 
    foldr (\(bnd, e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: NonEmpty (Binding, Recv) -> NonEmpty Binding -> NonEmpty (Binding, TaskExpr)
toExpr rVars sVars  = NE.map (second f) rVars
    where
        f (Recv _ ch) | HS.member ch ctrled = Var ch
        f (Recv i ch)  = Receive i ch
        ctrled = HS.fromList $ NE.toList sVars

-- assumption: ctrl before merging and as such there is only a single input
-- assumption: length inputs = length outputs 
mkCtrl :: CtrlInput -> Inputs -> Outputs -> Ctrl
mkCtrl ctrlInput inputs outputs = 
    Ctrl 
        sigStateInit 
        (NE.zip outputs inputs)
        sigStateRecv
        ctxtLoop 
        sigStateRenew
    where
        sigStateInit :: Binding -> TaskExpr -> TaskExpr
        sigStateInit bnd = Let bnd (Lit $ BoolLit False)

        sigStateRecv :: Binding -> TaskExpr -> TaskExpr -> TaskExpr
        sigStateRecv renew recvCode cont = 
            Let "sig" (Receive 0 ctrlInput) $
            Let "count" (L.Second $ Left "sig") $
            Stmt 
                (Cond (Var renew)
                    recvCode
                    (Lit UnitLit))
                cont
        
        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "count"

        sigStateRenew :: Binding -> TaskExpr
        sigStateRenew bnd = 
            Let "renew_next_time" (L.First $ Left "sig") $
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