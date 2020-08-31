module Ohua.Backend.Operators.Control where

import Ohua.Prelude

import Ohua.Backend.Lang as L


type CtrlInput = Binding

-- invariant: Int > 0
ctrl :: CtrlInput -> Int -> TaskExpr
ctrl ctrlInput numVars = 
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
            foldr (\i c -> f ("state" <> show i) (Receive 0 ("in" <> show i)) c) cont [i | i <- [1..numVars]]
        sendCode = foldr (\i c ->  Stmt (Send ("out" <> show i) ("state" <> show i)) c) (Lit UnitLit) [i | i <- [1..numVars]]

ctrlNoAssign :: CtrlInput -> Int -> TaskExpr
ctrlNoAssign ctrlInput numVars = 
    Let "init" 
        (Lambda [] $
            stateReceiveCode Let $
            Apply $ Stateless (QualifiedBinding (makeThrow []) "ctrl") $ map Var stateVars
        ) $
    Let "ctrl" 
        ( Lambda stateVars $
            Let "sig" (Receive 0 ctrlInput) $
            Let "renew_next_time" (L.First $ Left "sig") $
            Let "count" (L.Second $ Left "sig") $
            Stmt
            ( Repeat (Left "count")
                sendCode ) $
            Cond (Var "renew_next_time")
                (Apply $ Stateless (QualifiedBinding (makeThrow []) "init") [])
                (Apply $ Stateless (QualifiedBinding (makeThrow []) "ctrl") $ map Var stateVars)
            ) $
        Apply $ Stateless (QualifiedBinding (makeThrow []) "init") []
    where
        stateReceiveCode f cont = 
            foldr (\i c -> f ("state" <> show i) (Receive 0 ("in" <> show i)) c) cont [i | i <- [1..numVars]]
        sendCode = foldr (\i c ->  Stmt (Send ("out" <> show i) ("state" <> show i)) c) (Lit UnitLit) [i | i <- [1..numVars]]
        stateVars = ["state" <> show i | i <- [1..numVars]]