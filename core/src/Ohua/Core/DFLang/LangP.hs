{-# LANGUAGE DataKinds #-}
module Ohua.Core.DFLang.LangP where

import Ohua.Core.Prelude

data DFVar
    = DFEnvVar !Lit
    | DFVar !Binding
    deriving (Eq, Show, Generic)

instance Hashable DFVar

type StateBinding = Binding

-- | Annotations
data FunType 
    -- | a pure function
    = Pure
    -- | a state thread
    | State 
    deriving (Show, Eq, Generic)

data OutData 
    -- | Destructuring 
    = Destruct (NonEmpty OutData)
    -- | Copying of output data
    | Dispatch (NonEmpty Binding)
    deriving (Show, Eq, Generic)

-- | The applicative normal form with the ops resolved.
--   (a function with a single result)
data App :: FunType -> Type where
    PureFun :: Binding -> QualifiedBinding -> NonEmpty DFVar -> App 'Pure
    StateFun :: (Binding, Binding) -> QualifiedBinding -> StateBinding -> NonEmpty DFVar -> App 'State
    -- TODO define the builtin functions right here:
    -- SMap :: Binding -> '[Binding, Binding] -> App 'SMapFun
    -- If
    -- (no need to introduce Nth here at all!)

-- | The applicative normal form with the ops resolved.
--   (a function with output destructuring and dispatched result)
data DFApp :: FunType -> Type where
    PureDFFun :: OutData -> QualifiedBinding -> NonEmpty DFVar -> DFApp 'Pure
    StateDFFun :: (OutData, OutData) -> QualifiedBinding -> StateBinding -> NonEmpty DFVar -> DFApp 'State
    -- SMapFun
    -- Collect
    -- Ctrl
    -- IfFun
    -- Select
    -- RecurFun

data NormalizedExpr 
    = LetPureFun (App 'Pure) NormalizedExpr
    | LetStateFun (App 'State) NormalizedExpr
    | VarFun Binding
    deriving (Show, Eq, Generic)

data NormalizedDFExpr 
    = LetPureDFFun (App 'Pure) NormalizedDFExpr
    | LetStateDFFun (App 'State) NormalizedDFExpr
    | VarDFun Binding
    deriving (Show, Eq, Generic)

----------------------
-- Instances:
----------------------

deriving instance Show (App a)
deriving instance Eq (App a)

deriving instance Show (DFApp a)
deriving instance Eq (DFApp a)
