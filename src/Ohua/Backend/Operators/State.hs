module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)


type DataSizeInput = Binding
type StateInput = Binding
type StateOutput = Binding

type StateBnd = Binding

data STCLangSMap = 
    STCLangSMap
        (TaskExpr -> TaskExpr) -- init
        (TaskExpr -> TaskExpr) -- ctxt loop
        StateInput -- state receive
        StateOutput -- state emission
    deriving (Generic)

instance Eq STCLangSMap where
    (STCLangSMap _ _ inp out) == (STCLangSMap _ _ inp' out') = inp == inp' && out == out'

instance Hashable STCLangSMap where
    hashWithSalt s (STCLangSMap _ _ inp out) = s `hashWithSalt` inp `hashWithSalt` out

genSTCLangSMap :: STCLangSMap -> TaskExpr
genSTCLangSMap (STCLangSMap init ctxtLoop stateReceive emit) = 
    init $
    Stmt
    (
        ctxtLoop $
            Stmt (Receive 0 stateReceive) $
            Lit UnitLit
    ) $
    Let "s" (Receive 0 stateReceive)
        $ Send emit "s"

mkSTCLangSMap :: DataSizeInput -> StateInput -> StateOutput -> STCLangSMap
mkSTCLangSMap sizeInput = STCLangSMap init ctxtLoop
    where
        init :: TaskExpr -> TaskExpr
        init c =  
            Let "num" (Receive 0 sizeInput) $
            Let "toDrop" (Decrement "num") $
            Let "drops" (Generate "toDrop" UnitLit) c

        ctxtLoop :: TaskExpr -> TaskExpr
        ctxtLoop = Repeat $ Left "drops" 
