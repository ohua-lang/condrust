module Ohua.Backend.Operators.Function where

import Ohua.Prelude

import Ohua.Backend.Lang as L hiding (Function)

import qualified Data.List.NonEmpty as NE
import qualified Text.Show


data CallArg 
    = Arg Recv | Drop Recv | Converted TaskExpr
    deriving (Eq, Show)

data Function 
    = Pure
        QualifiedBinding
        [CallArg] -- call args
        -- Assumption: if there is no one that needs this result, then this should not be computed
        --             in the first place!
        Binding -- out
    | ST
        QualifiedBinding
        Recv -- state
        [CallArg] -- call args
        (Maybe Binding) -- state out
        (Maybe Binding) -- out

data FusableFunction 
    = PureFusable
        [CallArg]  -- data receive
        ([Binding] -> TaskExpr -> TaskExpr) -- application
        Send -- send result
    | STFusable
        Recv -- state receive
        [CallArg]  -- data receive
        (NonEmpty Binding -> TaskExpr -> TaskExpr) -- application
        (Maybe Send) -- send result
        (Maybe (Binding -> Send)) -- send state

instance Eq FusableFunction where
    (PureFusable inp _ out) == (PureFusable inp' _ out') = inp == inp && out == out'
    (STFusable sInp inp _ out sOut) == (STFusable sInp' inp' _ out' sOut') 
        = sInp == sInp' 
        && inp == inp' 
        && out == out' 
        && ((\f -> f "") <$> sOut) == ((\f -> f "") <$> sOut')

instance Show FusableFunction where
    show (PureFusable inp _ out) = "PureFusable { callArgs: " <> show inp <>" , output: " <> show out <> "}"
    show (STFusable sInp inp _ out sOut) = 
        "STFusable { sInp: " <> show sInp <> 
        " , callArgs: " <> show inp <> 
        " , output: " <> show out <> 
        " , stateOut: " <> show ((\f -> f "") <$> sOut) <>"}"

genFun :: FusableFunction -> TaskExpr
genFun = \case
    (PureFusable receives app send) ->
        loop receives $
            varsAndReceives receives $ 
            app (bnds 0 receives) $
            (\(Emit c d) -> Send c d) send
    (STFusable stateRecv receives app sendRes sendState) ->
        loop receives $
            varsAndReceives (Arg stateRecv : receives) $
            app (bndsNE $ Arg stateRecv :| receives) $
            foldr (\(Emit ch d) c -> Stmt (Send ch d) c) (Lit UnitLit) $ 
            catMaybes [sendRes, (\f -> f "var_0") <$> sendState]
    where
        bnds i = map (("var_" <>) . show . fst) . filter (\case (_, Drop _) -> False; _ -> True) . zip [i..]
        bndsNE = ("var_0" :|) . bnds 1 . NE.tail
        varsAndReceives rcvs cont = 
            foldr 
                ((\ (v, r) c -> Let v r c) . generateReceiveCode) 
                cont
                (zip [0 ..] rcvs)
        generateReceiveCode (idx, Arg (Recv cidx bnd)) = ("var_" <> show idx, Receive cidx bnd)
        generateReceiveCode (idx, Drop (Recv cidx bnd)) = ("_var_" <> show idx, Receive cidx bnd)
        generateReceiveCode (idx, Converted e) = ("var_" <> show idx, e)
        loop rcvs c = if any (\case Converted _ -> False; _ -> True) rcvs
                        then EndlessLoop c
                        else c

fun :: Function -> FusableFunction
fun = \case 
    (Pure funRef callArgs out) ->
        PureFusable
            callArgs
            (\args cont -> 
                Let "result" (Apply $ Stateless funRef $ map Var args)
                    cont)
            (out `Emit` "result")
    (ST funRef stateVar@(Recv _ stateBnd) callArgs stateOut out) ->
        STFusable
            stateVar
            callArgs
            (\args cont -> 
                Let "result" (Apply $ Stateful (NE.head args) funRef $ map Var $ NE.tail args)
                    cont)
            ((`Emit` "result") <$> out)
            (Emit <$> stateOut)

funReceives :: FusableFunction -> [Binding]
funReceives = 
    \case
        (PureFusable vars _ _) -> extract vars
        (STFusable (Recv _ bnd) vars _ _ _) -> bnd : extract vars
    where 
        extract = mapMaybe 
                    (\case 
                        (Arg (Recv _ bnd)) -> Just bnd
                        (Drop (Recv _ bnd)) -> Just bnd
                        (Converted _) -> Nothing)
        