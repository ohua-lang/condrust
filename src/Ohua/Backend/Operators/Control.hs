module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L


type CtrlInput = Binding
type Inputs = [Binding]
type Outputs = [Binding]

-- invariants:  lent Inputs == len Outputs, NonEmpty Inputs, NonEmpty Outputs
ctrl :: CtrlInput -> Inputs -> Outputs -> TaskExpr
ctrl ctrlInput inputs outputs = 
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