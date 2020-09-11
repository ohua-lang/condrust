module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L


type CtrlInput = Binding
type Inputs = NonEmpty Binding
type Outputs = NonEmpty Binding

data Ctrl 
    = Ctrl
        (TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty Recv) -- receive vars
        -- below here is computation code
        (Binding -> TaskExpr -> TaskExpr -> TaskExpr) -- signal state receive code
        (TaskExpr -> TaskExpr) -- ctxt loop
        [Binding] -- send vars out via channels
        (Binding -> TaskExpr) -- signal state renewal

data FusedCtrl
    = FusedCtrl
        (TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty (Binding, DataSource)) -- vars
        -- below here is computation code
        (Binding -> TaskExpr -> TaskExpr -> TaskExpr) -- signal state receive code
        (TaskExpr -> TaskExpr) -- signal state receive code
        TaskExpr -- comp
        (Binding -> TaskExpr) -- signal state renewal

fuse :: Ctrl -> FusedCtrl -> FusedCtrl
fuse 
    (Ctrl sigStateInit receiveVars sigStateRecv ctxtLoop sendVars sigStateRenewal) -- from
    (FusedCtrl sigStateInit' receiveVars' sigStateRecv' comp' sigStateRenewal') -- to
    = FusedCtrl
        sigStateInit
        receiveVars
        sigStateRecv
        (ctxtLoop $
            sigStateInit' "renew" $
            initVarsExpr' $
            While (Not $ Var "renew") $
                sigStateRecv' "renew" receiveVarsExpr' $
                Stmt
                    comp'
                    sigStateRenewal' "renew")
        sigStateRenewal
    where
        initVarsExpr' = initVarsExpr receiveVars' sendVars
        receiveVarsExpr' = receiveVarsExpr receiveVars' sendVars

fuseFun :: Ctrl -> FusableFunction -> FusedCtrl
fuseRun (Ctrl sigStateInit receiveVars sigStateRecv ctxtLoop sendVars sigStateRenewal)
        f
    = case f of 
        (Pure args app res) -> 
            FusedCtrl
                sigStateInit
                receiveVars
                sigStateRecv
                ( ctxtLoop $
                    gen $ 
                    PureFusable (map f args) app res
                )
                sigStateRenewal
        (ST args app stateRes res) ->
            FusedCtrl
                sigStateInit
                vars
                sigStateRecv
                ( ctxtLoop $
                    gen $ 
                        STFusable (NE.map f args) app stateRes res
                )
                sigStateRenewal
        where
            f (Left (Recv _ ch)) | HS.member ch ctrled = Var ch
            f (Right e)  = e
            ctrled = HS.fromList sendVars

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
    (Ctrl sigStateInit receiveVars sigStateRecv ctxtLoop sendVars sigStateRenewal)
    (Ctrl _ receiveVars' _ _ sendVars' _)
    = Ctrl
        sigStateInit
        (receiveVars <> receiveVars')
        sigStateRecv
        ctxtLoop
        (sendVars <> sendVars')
        sigStateRenewal

gen :: Ctrl -> TaskExpr
gen (Ctrl sigStateInit receiveVars sigStateRecv ctxtLoop sendVars sigStateRenewal) = 
    sigStateInit "renew" $
    initVarsExpr' $
    EndlessLoop $
        sigStateRecv' "renew" receiveVarsExpr' $
        Stmt
            (ctxtLoop $ 
                toCode sendVars)
            sigStateRenewal "renew"
    where
        initVarsExpr' = initVarsExpr receiveVars []
        receiveVarsExpr' = receiveVarsExpr receiveVars []

initVarsExpr rVars sVars cont = foldr (\(bnd, e) c -> Let bnd e c) (Lit UnitLit) $ toExpr rVars sVars
receiveVarsExpr rVars sVars = foldr (\(bnd, e) c -> Stmt (Assign bnd e) c) (Lit UnitLit) $ toExpr rVars sVars

toExpr :: NonEmpty Recv -> [Binding] -> NonEmpty TaskExpr
toExpr rVars sVars  = map f rVars
    where
        f (Recv _ ch) | HS.member ch ctrled = Var ch
        f (Recv i ch)  = Receive i ch
        ctrled = HS.fromList sVars

-- assumption: ctrl before merging and as such there is only a single input
mkCtrl :: CtrlInput -> Inputs -> Outputs -> Ctrl
mkCtrl ctrlInput inputs outputs = 
    Ctrl 
        sigStateInit 
        (NE.fromList $ map (Recv 0) inputs)
        sigStateRecv
        ctxtLoop 
        outputs
        sigStateRenew
    where
        sigStateInit :: Binding -> TaskExpr -> TaskExpr
        sigStateInit bnd = Let bnd (Lit $ BoolLit False)

        sigStateRecv :: Binding -> TaskExpr -> TaskExpr -> TaskExpr
        sigStateRecv renew recvCode cont = 
            Let "sig" (Receive 0 ctrlInput) $
            Let "count" (L.Second $ Left "sig") $
            Stmt $
                Cond (Var renew)
                    recvCode
                    (Lit UnitLit) 
        
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