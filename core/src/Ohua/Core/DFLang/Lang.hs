{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
module Ohua.Core.DFLang.Lang where

import Ohua.Core.Prelude
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated

import qualified Data.List.NonEmpty as NE


data BindingType = State | Data deriving (Show, Eq, Generic, Lift)

data ABinding :: BindingType -> Type where
    DataBinding :: Binding -> ABinding 'Data
    StateBinding :: Binding -> ABinding 'State

deriving instance Show (ABinding a)
deriving instance Eq (ABinding a) 
deriving instance Lift (ABinding a) 

data OutData :: BindingType -> Type where
    -- | Direct output
    Direct :: ABinding b -> OutData b
    -- | Destructuring 
    Destruct :: NonEmpty (OutData b) -> OutData b
    -- | Copying of output data
    Dispatch :: NonEmpty (ABinding b) -> OutData b
    deriving (Show, Eq, Generic, Lift)

data DFVar
    = DFEnvVar !Lit
    | DFVar (ABinding 'Data) -- TODO should be polymorph in the annotation
    deriving (Eq, Show, Generic, Lift)

-- | Annotations for the first normal formal
data FunANF 
    -- | a pure function
    = Fun
    -- | a state thread
    | ST
    deriving (Show, Eq, Generic, Lift)

-- -- | Annotations for the second normal formal
-- data DataflowANF 
--     -- | a pure function
--     = DfFun
--     -- | a state thread
--     | DfST
--     deriving (Show, Eq, Generic, Lift)

-- | The applicative normal form with the ops resolved.
--   (a function with a single result)
data App :: FunANF -> Type where
    -- TODO To do this properly, I would actually have to state that certain pure functions
    --      return data that becomes state. They are the state init functions. (This is really important in CodeGen.)
    --      Then this should propagate through the type system an make sure that this state is used only
    --      as a state. Currently, we really only tag the types of an app with almost no implication on the
    --      whole expression. This would then immediately remove the unwrapABnd function.
    PureFun :: forall (b::BindingType).
        ABinding b -> QualifiedBinding -> NonEmpty DFVar -> App 'Fun
    StateFun :: (Maybe (ABinding 'State), ABinding 'Data) -> QualifiedBinding -> ABinding 'State -> NonEmpty DFVar -> App 'ST
    -- TODO define the builtin functions right here:
    -- SMap :: Binding -> '[Binding, Binding] -> App 'SMapFun
    -- If
    -- (no need to introduce Nth here at all!)

-- | The applicative normal form with the ops resolved.
--   (a function with output destructuring and dispatched result)
data DFApp :: FunANF -> Type where
    PureDFFun ::  forall (b::BindingType).
        OutData b -> QualifiedBinding -> NonEmpty DFVar -> DFApp 'Fun
    StateDFFun :: (Maybe (OutData 'State), OutData 'Data) -> QualifiedBinding -> ABinding 'State -> NonEmpty DFVar -> DFApp 'ST
    -- SMapFun
    -- Collect
    -- Ctrl
    -- IfFun
    -- Select
    -- RecurFun

data NormalizedExpr 
    = LetPureFun (App 'Fun) NormalizedExpr
    | LetStateFun (App 'ST) NormalizedExpr
    | VarFun Binding
    deriving (Show, Eq, Generic)

-- TODO I believe this is not yet clean enough because it essentially says that two transformations have been performed:
--      destructuring was resolved and output replication was applied.
--      But I hope that the former transformation goes away completely once ALang has been refined and the transformations
--      that introduce nodes that require output destructuring have been moved to this normal form.
data NormalizedDFExpr 
    = LetPureDFFun (DFApp 'Fun) NormalizedDFExpr
    | LetStateDFFun (DFApp 'ST) NormalizedDFExpr
    | VarDFFun Binding
    deriving (Generic)

----------------------------
-- Accessor functions
----------------------------

outsApp :: App a -> NonEmpty Binding
outsApp (PureFun out _ _) = unwrapABnd out :| []
outsApp (StateFun (Nothing, out) _ _ _) = unwrapABnd out :| []
outsApp (StateFun (Just stateOut, out) _ _ _) = unwrapABnd stateOut :| [unwrapABnd out]

outsDFApp :: DFApp a -> NonEmpty Binding
outsDFApp (PureDFFun out _ _) = outBnds out
outsDFApp (StateDFFun (Nothing, out) _ _ _) = outBnds out
outsDFApp (StateDFFun (Just stateOut, out) _ _ _) = outBnds stateOut <> outBnds out

outBnds :: OutData a -> NonEmpty Binding
outBnds (Destruct outs) = sconcat $ NE.map outBnds outs
outBnds (Dispatch outs) = NE.map unwrapABnd outs
outBnds (Direct bnd) = unwrapABnd bnd :| []

insApp :: App a -> [Binding]
insApp (PureFun _ _ ins) = extractBndsFromInputs ins
insApp (StateFun _ _ _ ins) = extractBndsFromInputs ins

insDFApp :: DFApp a -> [Binding]
insDFApp (PureDFFun _ _ ins) = extractBndsFromInputs ins
insDFApp (StateDFFun _ _ _ ins) = extractBndsFromInputs ins

extractBndsFromInputs :: NonEmpty DFVar -> [Binding]
extractBndsFromInputs = 
    mapMaybe  (\case DFVar bnd -> Just $ unwrapABnd bnd; _ -> Nothing) . NE.toList

fnApp :: App a -> QualifiedBinding
fnApp (PureFun _ f _) = f
fnApp (StateFun _ f _ _) = f

fnDFApp :: DFApp a -> QualifiedBinding
fnDFApp (PureDFFun _ f _) = f
fnDFApp (StateDFFun _ f _ _) = f

unwrapABnd :: ABinding a -> Binding
unwrapABnd (DataBinding bnd) = bnd
unwrapABnd (StateBinding bnd) = bnd

----------------------
-- Instances:
----------------------

deriving instance Show (App a)
-- This just does not work because of the rank-2 type in the GADT. The type parameter must match, otherwise
-- this results in a compile-time error while this is actually a runtime equality check!
-- instance Eq (App a) where
--     (PureFun out fn inp) == (PureFun out' fn' inp') = out `outEq` out' && fn == fn' && inp == inp'
--         where
--             outEq :: ABinding b -> ABinding b -> Bool
--             outEq = (==)
--     (StateFun out fn stateIn inp) == (StateFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- So we remove the compile-time safety here and resort entirely to the runtime check:
instance Eq (App a) where
    (PureFun out fn inp) == (PureFun out' fn' inp') = unwrapABnd out == unwrapABnd out' && fn == fn' && inp == inp'
    (StateFun out fn stateIn inp) == (StateFun out' fn' stateIn' inp') =
        out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- We should think about these implications a little harder though!
deriving instance Lift (App a)

-- deriving instance Eq NormalizedExpr
deriving instance Lift NormalizedExpr
makeBaseFunctor ''NormalizedExpr
deriving instance Lift a => Lift (NormalizedExprF a)

instance Plated NormalizedExpr where
  plate = gplate

deriving instance Show (DFApp a)
-- instance Eq (DFApp a) where
--     (PureDFFun out fn inp) == (PureDFFun out' fn' inp') = out == out' && fn == fn' && inp == inp'
--     (StateDFFun out fn stateIn inp) == (StateDFFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- This does not work for the same reasons as in the case of App a.
instance Eq (DFApp a) where
    (PureDFFun out fn inp) == (PureDFFun out' fn' inp') = outBnds out == outBnds out' && fn == fn' && inp == inp'
    (StateDFFun out fn stateIn inp) == (StateDFFun out' fn' stateIn' inp') =
        out == out' && fn == fn' && stateIn == stateIn' && inp == inp'

deriving instance Lift (DFApp a)

deriving instance Show NormalizedDFExpr
deriving instance Eq NormalizedDFExpr
deriving instance Lift NormalizedDFExpr
makeBaseFunctor ''NormalizedDFExpr
deriving instance Lift a => Lift (NormalizedDFExprF a)
