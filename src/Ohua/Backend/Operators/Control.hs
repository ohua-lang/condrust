module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L


type CtrlInput = Binding
type Inputs = NonEmpty Binding
type Outputs = NonEmpty Binding

data Com = Com
            Binding
            (TaskExpr -> TaskExpr)


data Ctrl = 
    Ctrl
        (TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty RVar) -- var state init
        -- below here is computation code
        (TaskExpr -> TaskExpr) -- computation
        (NonEmpty RVar) -- receive var(s) from channels
        (TaskExpr -> TaskExpr) -- ctxt loop
        (NonEmpty Com) -- send vars out via channels
        TaskExpr -- signal state renewal
    FusedCtrl
        (TaskExpr -> TaskExpr) -- signal state init
        (NonEmpty RVar) -- var state init
        -- below here is computation code
        (TaskExpr -> TaskExpr) -- computation
        (NonEmpty RVar) -- receive var(s) from channels
        (TaskExpr -> TaskExpr) -- ctxt loop
        TaskExpr -- computation
        TaskExpr -- signal state renewal

fuseFun :: Ctrl -> TaskExpr -> TaskExpr
fuseRun (Ctrl sigStateInit varStateInit sigStateRecv receiveVars ctxtLoop sendVars sigStateRenewal)
        comp
        = 
        Ctrl sigStateInit varStateInit sigStateRecv receiveVars ctxtLoop comp sigStateRenewal

fuse :: Ctrl -> FusedCtrl -> Ctrl
fuse 
    (Ctrl sigStateInit varStateInit sigStateRecv receiveVars ctxtLoop _ sigStateRenewal) -- from
    (Ctrl sigStateInit' varStateInit' sigStateRecv' receiveVars' ctxtLoop' comp sigStateRenewal') -- to
    = FusedCtrl
        (sigStateInit sigStateInit')
        (varStateInit :| externalVarStateInits)
        sigStateRecv
        receiveVars
        (ctxtLoop $ 
            sigStateRecv' $
            remainingReceiveVars $
            ctxtLoop' $
                comp
                sigStateRenewal')
        (Lit UnitLit)
        sigStateRenewal
    where
        missing xs ys =
            let a = HS.fromList $ map fst xs
                b = HS.fromList $ map fst ys
                c = HS.difference b a
            in filter (\(v,_) -> HS.member v c) ys 
        externalVarStateInits = missing varStateInit varStateInit'
        externalReceiveVars = missing receiveVars receiveVars'
        remainingReceiveVars cont =
            foldr (\(Com _ e) c -> e c) cont externalReceiveVars

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
    (Ctrl sigStateInit varStateInit sigStateRecv receiveVars ctxtLoop sendVars sigStateRenewal)
    (Ctrl _ varStateInit' _ receiveVars' _ sendVars' _)
    = Ctrl
        sigStateInit
        (varStateInit :| varStateInit')
        sigStateRecv
        (receiveVars :| receiveVars')
        ctxtLoop
        (sendVars :| sendVars')
        sigStateRenewal

gen :: Ctrl -> TaskExpr
gen (Ctrl sigStateInit varStateInit sigStateRecv receiveVars ctxtLoop sendVars sigStateRenewal) = 
    sigStateInit "renew" $
    toCode varStateInit $
    EndlessLoop $
        sigStateRecv $
        toCode receiveVars $
        Stmt 
        ( ctxtLoop 
            toCode sendVars
        ) $
        sigStateRenewal "renew"
    where
        toCode coms cont = foldr (\(Com _ e) c -> e c) cont coms

mkCtrl :: CtrlInput -> Inputs -> Outputs -> Ctrl
mkCtrl ctrlInput inputs outputs = 
    Ctrl sigStateInit varStateInit sigStateRecv receiveVars ctxtLoop sendCode sigStateRenew
    where
        sigStateInit :: Binding -> TaskExpr -> TaskExpr
        sigStateInit bnd = Let bnd (Lit $ BoolLit False)

        varStateInit :: TaskExpr -> TaskExpr
        varStateInit = varReceiveCode Let -- this declares the state that this operator needs.

        sigStateRecv :: TaskExpr -> TaskExpr
        sigStateRecv = 
            Let "sig" (Receive 0 ctrlInput) $
            Let "count" (L.Second $ Left "sig")

        receiveVars :: TaskExpr -> TaskExpr
        receiveVars = 
            Stmt $
                Cond (Var renew)
                    (varReceiveCode (\v r c -> Stmt (Assign v r) c) $ Lit UnitLit)
                    (Lit UnitLit) 
        
        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "count"

        sigStateRenew :: Binding -> TaskExpr
        sigStateRenew bnd = 
            Let "renew_next_time" (L.First $ Left "sig") $
            Assign bnd $ Var "renew_next_time"

        varReceiveCode f cont = 
            -- FIXME the generation of the variable names needs to change!
            map 
                (\(i,input) -> Com input $ f ("var" <> show i) (Receive 0 input)) $ 
                zip [1..] inputs
        sendCode = 
            NE.fromList $ 
            map (\(o, output) -> Com output $ Stmt $ Send output ("var" <> show o))
            $ zip [1..] outputs



-- invariants:  length Inputs == length Outputs, NonEmpty Inputs, NonEmpty Outputs
ctrl' :: CtrlInput -> Inputs -> Outputs -> TaskExpr
ctrl' ctrlInput inputs outputs = 
    Let "renew" (Lit $ BoolLit False) $
    stateReceiveCode Let $
    EndlessLoop $
        Let "sig" (Receive 0 ctrlInput) $
        Let "renew_next_time" (L.First $ Left "sig") $
        Let "count" (L.Second $ Left "sig") $
        Stmt 
        ( Cond (Var "renew")
            (stateReceiveCode (\v r c -> Stmt (Assign v r) c) $ Lit UnitLit)
            (Lit UnitLit) ) $
        Stmt
        ( Repeat (Left "count")
            sendCode ) $
        Assign "renew" (Var "renew_next_time")
    where
        stateReceiveCode f cont = 
            foldr (\(i,input) c -> f ("state" <> show i) (Receive 0 input) c) cont $ zip [1..] inputs
        sendCode = foldr (\(o,output) c ->  Stmt (Send output ("state" <> show o)) c) (Lit UnitLit) $ zip [1..] outputs

ctrlNoAssign :: CtrlInput -> Inputs -> Outputs -> (TaskExpr, NonEmpty (Function TaskExpr))
ctrlNoAssign ctrlInput inputs outputs = 
    let initFun = 
            Function (QualifiedBinding (makeThrow []) "init") [] $
                stateReceiveCode Let $
                Apply $ Stateless (QualifiedBinding (makeThrow []) "ctrl") $ map Var stateVars
        ctrlFun =
            Function (QualifiedBinding (makeThrow []) "ctrl") stateVars $
                Let "sig" (Receive 0 ctrlInput) $
                Let "renew_next_time" (L.First $ Left "sig") $
                Let "count" (L.Second $ Left "sig") $
                Stmt
                ( Repeat (Left "count")
                    sendCode ) $
                Cond (Var "renew_next_time")
                    (Apply $ Stateless (QualifiedBinding (makeThrow []) "init") [])
                    (Apply $ Stateless (QualifiedBinding (makeThrow []) "ctrl") $ map Var stateVars)
        expr = Apply $ Stateless (QualifiedBinding (makeThrow []) "init") []
    in (expr, [initFun, ctrlFun])
    where
        stateReceiveCode f cont = 
            foldr (\(input, i) c -> f ("state" <> show i) (Receive 0 input) c) cont $ zip inputs [1..]
        sendCode = foldr (\(output, o) c ->  Stmt (Send output ("state" <> show o)) c) (Lit UnitLit) $ zip outputs [1..]
        stateVars = ["state" <> show i | i <- [1..(length inputs)]]