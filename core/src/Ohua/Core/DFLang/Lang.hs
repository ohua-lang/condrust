{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Core.DFLang.Lang where

import Ohua.Core.Prelude
import Ohua.Types.Vector as V hiding (map)

import Ohua.Core.DFLang.Refs as DFLangRefs (recurFun)

import qualified Data.List.NonEmpty as NE


data BindingType = State | Data deriving (Show, Eq, Generic)

data ABinding :: BindingType -> Type where
    DataBinding :: Binding -> ABinding 'Data
    StateBinding :: Binding -> ABinding 'State

deriving instance Show (ABinding a)
deriving instance Eq (ABinding a) 

data OutData :: BindingType -> Type where
    -- | Direct output
    Direct :: ABinding b -> OutData b
    -- | Destructuring 
    Destruct :: NonEmpty (OutData b) -> OutData b
    -- | Copying of output data
    Dispatch :: NonEmpty (ABinding b) -> OutData b
    deriving (Show, Eq, Generic)

-- data DFVar ty
--     = DFEnvVar ty (Lit ty)
--     | DFVar ty (ABinding 'Data) -- TODO should be polymorph in the annotation
--     | DFStateVar ty (ABinding 'State) -- TODO should be polymorph in the annotation
--     deriving (Show, Generic)

data DFVar (semType::BindingType) (ty::Type) :: Type where
    DFEnvVar :: ArgType ty -> Lit ty -> DFVar 'Data ty
    DFVar :: ArgType ty -> ABinding a -> DFVar a ty
    -- DFStateVar :: ArgType ty -> ABinding 'State -> DFVar 'State ty

-- | Annotations for functions
data FunANF :: Type where
    -- | a pure function
    Fun :: FunANF
    -- | a state thread
    ST :: FunANF
    -- | a built-in function
    BuiltIn :: FunANF
    deriving (Show, Eq, Generic)

-- (TODO: As a matter of fact, it is possible to prevent this annotation all together and instead
--  do what singletons would do: promote App directly. Then we would not have types 'Fun and 'ST but
--  'PureFun and 'StateFun. This may be much simpler because then we do not have the type-level function
--  in all of the types!)

-- (TODO: It becomes all clear no how this looks: both App's will have to be extended with the
--        specific functions that the form incorporates. Question is: can ALang also just be encoded as what it is
--        while the let itself remindes defined like it is now?!)

-- | The applicative normal form with the ops resolved.
--   (a function with a single result)
data App (f::FunANF) (ty::Type) :: Type where
    -- TODO To do this properly, I would actually have to state that certain pure functions
    --      return data that becomes state. They are the state init functions. (This is really important in CodeGen.)
    --      Then this should propagate through the type system an make sure that this state is used only
    --      as a state. Currently, we really only tag the types of an app with almost no implication on the
    --      whole expression. This would then immediately remove the unwrapABnd function.
    PureFun :: ABinding b -> QualifiedBinding -> NonEmpty (DFVar 'Data ty) -> App 'Fun ty
    StateFun :: (Maybe (ABinding 'State), ABinding 'Data) 
            -> QualifiedBinding -> DFVar 'State ty -> NonEmpty (DFVar 'Data ty) -> App 'ST ty
    -- TODO define the builtin functions right here:
    -- SMap :: Binding -> '[Binding, Binding] -> App 'SMapFun
    -- If
    -- (no need to introduce Nth here at all!)

-- | The applicative normal form with the ops resolved.
--   (a function with output destructuring and dispatched result)
data DFApp (f::FunANF) (ty::Type) :: Type where
    PureDFFun :: OutData b -> QualifiedBinding -> NonEmpty (DFVar 'Data ty) -> DFApp 'Fun ty
    -- FIXME This type is actually incorrect. The data output must not be used necessarily
    --       when the state output is!
    StateDFFun :: (Maybe (OutData 'State), OutData 'Data)
                -> QualifiedBinding -> DFVar 'State ty -> NonEmpty (DFVar 'Data ty) -> DFApp 'ST ty
    RecurFun :: --(n ~ 'Succ m) =>
            -- (final) result out
            OutData b ->
            -- | recursion control output
            OutData 'Data ->
            -- | recursion args outputs
            Vec n (OutData b) ->
            -- | initial inputs
            Vec n (DFVar a ty) ->
            -- | recursion args inputs
            Vec n (DFVar a ty) ->
            -- | recursion condition input
            DFVar 'Data ty ->
            -- (final) result in
            DFVar 'Data ty ->
            DFApp 'BuiltIn ty
    -- SMapFun
    -- Collect
    -- Ctrl
    -- IfFun
    -- Select
    -- RecurFun

data Expr (fun:: FunANF -> Type -> Type) (ty::Type) :: Type where
     Let :: (Show (fun a ty), Function (fun a ty)) => fun a ty -> Expr fun ty -> Expr fun ty
     -- FIXME this also should probably have a BindingType!
     Var :: Binding -> Expr fun ty

----------------------------
-- Accessor functions
----------------------------

outsApp :: App ty a -> NonEmpty Binding
outsApp (PureFun out _ _) = unwrapABnd out :| []
outsApp (StateFun (Nothing, out) _ _ _) = unwrapABnd out :| []
outsApp (StateFun (Just stateOut, out) _ _ _) = unwrapABnd stateOut :| [unwrapABnd out]

outsDFApp :: DFApp ty a -> NonEmpty Binding
outsDFApp (PureDFFun out _ _) = outBnds out
outsDFApp (StateDFFun (Nothing, out) _ _ _) = outBnds out
outsDFApp (StateDFFun (Just stateOut, out) _ _ _) = outBnds stateOut <> outBnds out
outsDFApp (RecurFun result ctrl recurs _ _ _ _) =
    let (x:|xs) = outBnds result
    in (x :| (xs <> join (map (NE.toList . outBnds) $ V.toList recurs))) <> outBnds ctrl

outBnds :: OutData a -> NonEmpty Binding
outBnds (Destruct o) = sconcat $ NE.map outBnds o
outBnds (Dispatch o) = NE.map unwrapABnd o
outBnds (Direct bnd) = unwrapABnd bnd :| []

insApp :: App ty a -> [Binding]
insApp (PureFun _ _ i) = extractBndsFromInputs $ NE.toList i
insApp (StateFun _ _ (DFVar _ s) i) = unwrapABnd s : extractBndsFromInputs (NE.toList i)

insDFApp :: DFApp ty a -> [Binding]
insDFApp (PureDFFun _ _ i) = extractBndsFromInputs $ NE.toList i
insDFApp (StateDFFun _ _ (DFVar _ s) i) = unwrapABnd s : extractBndsFromInputs (NE.toList i)
insDFApp (RecurFun _ _ _ initIns recurs cond result) =
    extractBndsFromInputs (V.toList initIns) <>
    extractBndsFromInputs (V.toList recurs) <>
    extractBndsFromInputs [cond] <>
    extractBndsFromInputs [result]

extractBndsFromInputs :: [DFVar a ty] -> [Binding]
extractBndsFromInputs =
    mapMaybe  (\case DFVar _ bnd -> Just $ unwrapABnd bnd; _ -> Nothing)

fnApp :: App ty a -> QualifiedBinding
fnApp (PureFun _ f _) = f
fnApp (StateFun _ f _ _) = f

fnDFApp :: DFApp 'Fun a -> QualifiedBinding
fnDFApp (PureDFFun _ f _) = f

sfnDFApp :: DFApp 'ST a -> QualifiedBinding
sfnDFApp (StateDFFun _ f _ _) = f

unwrapABnd :: ABinding a -> Binding
unwrapABnd (DataBinding bnd) = bnd
unwrapABnd (StateBinding bnd) = bnd

----------------------
-- Instances:
----------------------

deriving instance Show (DFVar semTy ty)
deriving instance Eq (DFVar semTy ty)

deriving instance Show (App ty a)
-- deriving instance Eq (App ty a)
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
instance Eq (App ty a) where
    (PureFun out fn inp) == (PureFun out' fn' inp') = unwrapABnd out == unwrapABnd out' && fn == fn' && inp == inp'
    (StateFun out fn stateIn inp) == (StateFun out' fn' stateIn' inp') =
        out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- We should think about these implications a little harder though!
-- deriving instance Lift (App a)

deriving instance Show (NormalizedExpr ty)
-- deriving instance Eq (NormalizedExpr ty)
-- deriving instance Lift NormalizedExpr
-- makeBaseFunctor ''NormalizedExpr
-- deriving instance Lift a => Lift (NormalizedExprF a)

-- instance Plated NormalizedExpr where
--   plate = gplate

deriving instance Show (DFApp ty a)
-- deriving instance Eq (DFApp ty a)
-- instance Eq (DFApp a) where
--     (PureDFFun out fn inp) == (PureDFFun out' fn' inp') = out == out' && fn == fn' && inp == inp'
--     (StateDFFun out fn stateIn inp) == (StateDFFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- This does not work for the same reasons as in the case of App a.
-- instance Eq (DFApp a ty) where
--     (PureDFFun out fn inp) == (PureDFFun out' fn' inp') = outBnds out == outBnds out' && fn == fn' && inp == inp'
--     (StateDFFun out fn stateIn inp) == (StateDFFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'

-- deriving instance Lift (DFApp ty a)

deriving instance Show (NormalizedDFExpr ty)
-- deriving instance Eq NormalizedDFExpr
-- deriving instance Lift NormalizedDFExpr
-- makeBaseFunctor ''NormalizedDFExpr
-- deriving instance Lift a => Lift (NormalizedDFExprF a)

----------------------------------
-- Types and traversals/helpers
----------------------------------

type NormalizedExpr ty = Expr App ty
type NormalizedDFExpr ty = Expr DFApp ty
-- TODO
-- data family Expression (a::FunANF -> Type)
-- data instance Expression (a::FunANF -> Type) where
--     NormalizedExpr :: Expression App
--     NormalizedDFExpr :: Expression DFApp

class Function a where
    outBindings :: a -> NonEmpty Binding
    inBindings :: a -> [Binding]
    funRef :: a -> QualifiedBinding

instance Function (App ty a) where
    outBindings = outsApp
    inBindings = insApp
    funRef = fnApp

instance Function (DFApp ty a) where
    outBindings = outsDFApp
    inBindings = insDFApp
    funRef f =
        case f of
            PureDFFun{} -> fnDFApp f
            StateDFFun{} -> sfnDFApp f
            RecurFun{} -> DFLangRefs.recurFun

transformExpr :: forall ty.(NormalizedDFExpr ty -> NormalizedDFExpr ty) -> NormalizedDFExpr ty -> NormalizedDFExpr ty
transformExpr f = runIdentity . transformExprM go
    where
        go :: NormalizedDFExpr ty -> Identity (NormalizedDFExpr ty)
        go = pure . f

-- | This is a bottom-up traversal
transformExprM :: Monad m => (NormalizedDFExpr ty -> m (NormalizedDFExpr ty)) -> NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
transformExprM f (Let app cont) = f . Let app =<< transformExprM f cont
transformExprM f v@(Var _) = f v

-- | This is a top-down traversal
transformExprTDM :: Monad m => (NormalizedDFExpr ty -> m (NormalizedDFExpr ty)) -> NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
transformExprTDM f l = f l >>= (\case
                                   (Let app cont) -> Let app <$> transformExprTDM f cont
                                   v              -> return v)

mapFunsM :: Monad m => (forall a.DFApp a ty -> m (DFApp a ty)) -> NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
mapFunsM f (Let app cont) = Let <$> (f app) <*> mapFunsM f cont
mapFunsM _ v = return v

-- This is what I want!
-- paraExpr :: ('Let -> a -> a) -> ('Var -> a) -> NormalizedDFExpr -> a
-- paraExpr = undefined
