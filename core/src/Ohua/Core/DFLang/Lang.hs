{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, NoOverloadedLists #-}

module Ohua.Core.DFLang.Lang where

import qualified Data.List.NonEmpty as NE
import Ohua.Core.ALang.Refs as ALangRefs (recurFun, smapFun, ifFun)
import Ohua.Core.Prelude hiding (length)
import Ohua.Types.Vector as V hiding (map)
import qualified Data.HashSet as HS

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

data DFVar (semType :: BindingType) (ty :: Type) :: Type where
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
data App (f :: FunANF) (ty :: Type) :: Type where
  -- TODO To do this properly, I would actually have to state that certain pure functions
  --      return data that becomes state. They are the state init functions. (This is really important in CodeGen.)
  --      Then this should propagate through the type system an make sure that this state is used only
  --      as a state. Currently, we really only tag the types of an app with almost no implication on the
  --      whole expression. This would then immediately remove the unwrapABnd function.
  PureFun :: ABinding b -> FunRef ty -> NonEmpty (DFVar 'Data ty) -> App 'Fun ty
  StateFun ::
    (Maybe (ABinding 'State), ABinding 'Data) ->
    FunRef ty ->
    DFVar 'State ty ->
    NonEmpty (DFVar 'Data ty) ->
    App 'ST ty

-- TODO define the builtin functions right here:
-- SMap :: Binding -> '[Binding, Binding] -> App 'SMapFun
-- If
-- (no need to introduce Nth here at all!)

-- | The applicative normal form with the ops resolved.
--   (a function with output destructuring and dispatched result)
data DFApp (f :: FunANF) (ty :: Type) :: Type where
  PureDFFun :: OutData b -> FunRef ty -> NonEmpty (DFVar 'Data ty) -> DFApp 'Fun ty
  StateDFFun ::
    (Maybe (OutData 'State), Maybe (OutData 'Data)) ->
    FunRef ty ->
    DFVar 'State ty ->
    NonEmpty (DFVar 'Data ty) ->
    DFApp 'ST ty
  RecurFun :: --(n ~ 'Succ m) =>
  -- (final) result out
    OutData b ->
    -- | recursion control output
    Maybe (OutData 'Data) ->
    -- | recursion args outputs
    Vec n (OutData b) ->
    -- | initial inputs
    Vec n (DFVar a ty) ->
    -- | recursion args inputs
    Vec n (DFVar a ty) ->
    -- | recursion condition input
    DFVar 'Data ty ->
    -- | (final) result in
    DFVar 'Data ty ->
    DFApp 'BuiltIn ty
  SMapFun
       -- TODO the below output type is not strong enough because we want to say,
       --      that at least one of these things needs to be present.
       --      so we need a non-empty heterogeneous list (that is at most of length 3)
       --      and we need to be able to identify the different outputs in this list
       --      regardless of their position in the list.
    :: ( Maybe (OutData b)     -- ^ data out
         -- TODO subset types would be great here to show that this out data can never be
         --      destructured.
       , Maybe (OutData 'Data) -- ^ control out
       , Maybe (OutData 'Data) -- ^ size out for collect and stclangCollect
       )
    -> DFVar a ty  -- ^ data in
    -> DFApp 'Fun ty -- FIXME would be this: DFApp 'BuiltIn ty (fix when all IRs are fixed and I do not need to convert a Fun into a BuiltIn)
  IfFun
    :: (OutData 'Data, OutData 'Data)
    -> DFVar 'Data ty
    -> DFApp 'Fun ty -- FIXME would be this: DFApp 'BuiltIn ty (fix when all IRs are fixed and I do not need to convert a Fun into a BuiltIn)

-- Collect
-- Ctrl
-- Select

data Expr (fun :: FunANF -> Type -> Type) (ty :: Type) :: Type where
  Let :: (Show (fun a ty), Function (fun a ty)) => fun a ty -> Expr fun ty -> Expr fun ty
  -- FIXME this also should probably have a BindingType!
  Var :: Binding -> Expr fun ty

----------------------------
-- Accessor functions
----------------------------

renameABnd :: Binding -> ABinding a -> ABinding a
renameABnd b (DataBinding _) = DataBinding b
renameABnd b (StateBinding _) = StateBinding b

outsApp :: App ty a -> NonEmpty Binding
outsApp (PureFun out _ _) = unwrapABnd out :| []
outsApp (StateFun (Nothing, out) _ _ _) = unwrapABnd out :| []
outsApp (StateFun (Just stateOut, out) _ _ _) = unwrapABnd stateOut :| [unwrapABnd out]

outsDFApp :: DFApp ty a -> [Binding]
outsDFApp (PureDFFun out _ _) = NE.toList $ outBnds out
outsDFApp (StateDFFun (Nothing, Nothing) _ _ _) = []
outsDFApp (StateDFFun (Nothing, Just out) _ _ _) = NE.toList $ outBnds out
outsDFApp (StateDFFun (Just stateOut, Nothing) _ _ _) = NE.toList $ outBnds stateOut
outsDFApp (StateDFFun (Just stateOut, Just out) _ _ _) = NE.toList $ outBnds stateOut <> outBnds out
outsDFApp (RecurFun result ctrl recurs _ _ _ _) =
  let (x :| xs) = outBnds result
   in ( NE.toList $
          (x :| (xs <> join (map (NE.toList . outBnds) $ V.toList recurs)))
      )
        ++ maybe [] (NE.toList . outBnds) ctrl
outsDFApp (SMapFun (dOut, collectOut, sizeOut) _) =
  maybe [] (NE.toList . outBnds) dOut ++
  maybe [] (NE.toList . outBnds) collectOut ++
  maybe [] (NE.toList . outBnds) sizeOut
outsDFApp (IfFun (oTrue, oFalse) _) = NE.toList $ outBnds oTrue <> outBnds oFalse

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
  extractBndsFromInputs (V.toList initIns)
    <> extractBndsFromInputs (V.toList recurs)
    <> extractBndsFromInputs [cond]
    <> extractBndsFromInputs [result]
insDFApp (SMapFun _ dIn) = extractBndsFromInputs [dIn]
insDFApp (IfFun _ dIn) = extractBndsFromInputs [dIn]

extractBndsFromInputs :: [DFVar a ty] -> [Binding]
extractBndsFromInputs =
  mapMaybe (\case DFVar _ bnd -> Just $ unwrapABnd bnd; _ -> Nothing)

-- TODO refactor with above function
insAndTypesDFApp :: DFApp ty a -> [(ArgType a, Binding)]
insAndTypesDFApp (PureDFFun _ _ i) = extractBndsAndTypesFromInputs $ NE.toList i
insAndTypesDFApp (StateDFFun _ _ (DFVar sTyp s) i) = (sTyp, unwrapABnd s) : extractBndsAndTypesFromInputs (NE.toList i)
insAndTypesDFApp (RecurFun _ _ _ initIns recurs cond result) =
  extractBndsAndTypesFromInputs (V.toList initIns)
    <> extractBndsAndTypesFromInputs (V.toList recurs)
    <> extractBndsAndTypesFromInputs [cond]
    <> extractBndsAndTypesFromInputs [result]
insAndTypesDFApp (SMapFun _ dIn) = extractBndsAndTypesFromInputs [dIn]
insAndTypesDFApp (IfFun _ dIn) = extractBndsAndTypesFromInputs [dIn]

extractBndsAndTypesFromInputs :: [DFVar a ty] -> [(ArgType ty, Binding)]
extractBndsAndTypesFromInputs =
  mapMaybe (\case DFVar typ bnd -> Just $ (typ, unwrapABnd bnd); _ -> Nothing)

fnApp :: App ty a -> QualifiedBinding
fnApp (PureFun _ (FunRef f _ _) _) = f
fnApp (StateFun _ (FunRef f _ _) _ _) = f

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
  outBindings :: a -> [Binding]
  inBindings :: a -> [Binding]
  funRef :: a -> QualifiedBinding

instance Function (App ty a) where
  outBindings = NE.toList . outsApp
  inBindings = insApp
  funRef = fnApp

instance Function (DFApp ty a) where
  outBindings = outsDFApp
  inBindings = insDFApp
  funRef f =
    case f of
      (PureDFFun _ (FunRef fr _ _) _) -> fr
      (StateDFFun _ (FunRef fr _ _) _ _) -> fr
      RecurFun {} -> ALangRefs.recurFun
      SMapFun {} -> ALangRefs.smapFun
      IfFun {} -> ALangRefs.ifFun

transformExpr :: forall ty. (NormalizedDFExpr ty -> NormalizedDFExpr ty) -> NormalizedDFExpr ty -> NormalizedDFExpr ty
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
transformExprTDM f l =
  f l
    >>= ( \case
            (Let app cont) -> Let app <$> transformExprTDM f cont
            v -> return v
        )

-- | A top-down traversal
mapFunsM :: Monad m => (forall a. DFApp a ty -> m (DFApp a ty)) -> NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
mapFunsM f (Let app cont) = do
  app' <- f app
  cont' <- mapFunsM f cont
  return $ Let app' cont'
mapFunsM _ v = return v

mapFuns :: (forall a. DFApp a ty -> DFApp a ty) -> NormalizedDFExpr ty -> NormalizedDFExpr ty
mapFuns f e = runIdentity (mapFunsM (pure . f) e)

-- This is what I want!
-- paraExpr :: ('Let -> a -> a) -> ('Var -> a) -> NormalizedDFExpr -> a
-- paraExpr = undefined

length :: Expr semTy ty -> V.Nat
length (Let _ cont) = Succ $ length cont
length Var {} = Zero

countBindings :: NormalizedDFExpr ty -> V.Nat
countBindings (Var _) = Succ Zero
countBindings (Let app cont) =
  foldl (\acc _ -> Succ acc) (countBindings cont) $ insDFApp app ++ outsDFApp app

usedBindings :: NormalizedDFExpr ty -> [Binding]
usedBindings (Let app cont) = insDFApp app ++ usedBindings cont
usedBindings (Var result) = [result]


data SubstitutionStrategy = FirstOccurrence | All

-- | Assumes SSA
-- TODO How can we encode this into the type system other than with de Bruijn indexes?
--      I guess we could define a de Bruijn index on the type level that carries a name on the term level.
substitute :: forall ty. SubstitutionStrategy -> (Binding, Binding) -> NormalizedDFExpr ty -> NormalizedDFExpr ty
substitute strat (from,to) e = evalState (mapFunsM go e) False
  where
    go :: DFApp a ty -> State Bool (DFApp a ty)
    go (PureDFFun o f dIns) = check =<< PureDFFun o f <$> mapM replace dIns
    go (StateDFFun o f sIn dIns) = check =<< StateDFFun o f sIn <$> mapM replace dIns
    go r@RecurFun{} = return r -- we do not replace these because RecurFun has variables that not yet defined.
                               -- we need a better representation for these.
    go (SMapFun o i) = check =<< SMapFun o <$> replace i
    go (IfFun o i) = check =<< IfFun o <$> replace i

    replace :: DFVar t ty -> State Bool (DFVar t ty)
    replace d@(DFVar t bnd) | unwrapABnd bnd == from = do
                              s <- get
                              case (strat, s) of
                                (FirstOccurrence, True)  -> return d
                                (FirstOccurrence, False) -> DFVar t (renameABnd to bnd) <$ put True
                                _ -> return $ DFVar t $ renameABnd to bnd
    replace d = return d

    check :: DFApp a ty -> State Bool (DFApp a ty)
    check app = checkDefined app >> return app

    checkDefined :: DFApp a ty -> State Bool ()
    checkDefined f =
      if HS.member from $ HS.fromList $ outBindings f
      then put True
      else return ()
