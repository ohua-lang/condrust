{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, NoOverloadedLists #-}

module Ohua.Core.DFLang.Lang where

import qualified Data.List.NonEmpty as NE
import Ohua.Core.ALang.Refs as ALangRefs (recurFun, smapFun, ifFun, collect, select, ctrl)
import Ohua.Core.ALang.Lang as ALang (TypedBinding(..), asBnd, asType)
import Ohua.Core.Prelude hiding (length)
import Ohua.Types.Vector as V hiding (map)
import qualified Data.HashSet as HS

data BindingType = State | Data deriving (Show, Eq, Generic)

data ATypedBinding (bType :: BindingType) (ty :: Type) :: Type where
  DataBinding :: TypedBinding ty -> ATypedBinding 'Data ty
  StateBinding :: TypedBinding ty -> ATypedBinding 'State ty

deriving instance Show (ATypedBinding a ty)

deriving instance Eq (ATypedBinding a ty)

data OutData (bType :: BindingType) (ty :: Type) :: Type  where
  -- | Direct output
  Direct :: ATypedBinding bType ty -> OutData bType ty
  -- | Destructuring
  Destruct :: NonEmpty (OutData bType ty) -> OutData bType ty
  -- | Copying of output data
  Dispatch :: NonEmpty (ATypedBinding bType ty) -> OutData bType ty
  deriving (Show, Eq, Generic)


-- ToDo : I think we should remove the indirection via the ATypedBinding and instead use an extended binding type
-- i.e. BindingType = State | Data | Env to directly type DFVar
data DFVar (semType :: BindingType) (ty :: Type) :: Type where
  DFEnvVar :: ArgType ty -> Lit ty -> DFVar 'Data ty
  DFVar :: ATypedBinding a ty -> DFVar a ty
  -- DFNatVar :: ATypedBinding a -> DFVar a (TypeNat ty) 
  -- DFBoolVar ::  ATypedBinding a -> DFVar a 'TypeBool
  DFStateVar :: ATypedBinding 'State ty -> DFVar 'State ty

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
  PureFun :: DFVar bTy ty -> FunRef ty -> NonEmpty (DFVar 'Data ty) -> App 'Fun ty
  StateFun ::
    (Maybe (ATypedBinding 'State ty), ATypedBinding 'Data ty) ->
    FunRef ty ->
    DFVar 'State ty ->
    NonEmpty (DFVar 'Data ty) ->
    App 'ST ty

-- | The applicative normal form with the ops resolved.
--   (a function with output destructuring and dispatched result)
data DFApp (f :: FunANF) (ty :: Type) :: Type where
  PureDFFun :: OutData b ty -> FunRef ty -> NonEmpty (DFVar 'Data ty) -> DFApp 'Fun ty
  StateDFFun ::
    (Maybe (OutData 'State ty ), Maybe (OutData 'Data ty)) ->
    FunRef ty ->
    DFVar 'State ty ->
    NonEmpty (DFVar 'Data ty) ->
    DFApp 'ST ty

  RecurFun :: --(n ~ 'Succ m) =>
  -- (final) result out
    OutData b ty ->
    -- | recursion control output
    Maybe (OutData 'Data ty) ->
    -- | recursion args outputs
    Vec n (OutData b ty) ->
    -- | initial inputs
    Vec n (DFVar a ty) ->
    -- | recursion args inputs
    Vec n (DFVar a ty) ->
    -- | recursion condition input
    DFVar 'Data ty ->
    -- | (final) result in
    DFVar 'Data ty ->
    DFApp 'Fun ty -- DFApp 'BuiltIn ty , commented BuiltIn out cause it's currently useless and messes up my monads

  SMapFun ::
    -- TODO the below output type is not strong enough because we want to say,
    --      that at least one of these things needs to be present.
    --      so we need a non-empty heterogeneous list (that is at most of length 3)
    --      and we need to be able to identify the different outputs in this list
    --      regardless of their position in the list
    -- TODO subset types would be great here to show that this out data can never be destructured.
    -- | ( data out, control out, size out for collect and stclangCollect)
    ( Maybe (OutData b ty)
    , Maybe (OutData 'Data ty) 
    , Maybe (OutData 'Data ty)
    ) -> 
    -- | data in
    DFVar a ty -> 
    DFApp 'Fun ty -- FIXME would be this: DFApp 'BuiltIn ty (fix when all IRs are fixed and I do not need to convert a Fun into a BuiltIn)
  
  IfFun
    :: (OutData 'Data ty, OutData 'Data ty )
    -> DFVar 'Data ty
    -> DFApp 'Fun ty -- FIXME would be this: DFApp 'BuiltIn ty (fix when all IRs are fixed and I do not need to convert a Fun into a BuiltIn)

  CollectFun
    -- collect :: nat ->  A -> [A]
    -- output is a list of lenght first input of elements, second input
    :: (OutData 'Data ty) -> 
    -- | first input as a nat
    DFVar 'Data ty -> 
    -- | second input is an elemt, which is actually always a ()
    DFVar 'Data ty -> 
    DFApp 'Fun ty 

  CtrlFun 
    -- ctrl:: (bool, nat) -> A -> A
    :: OutData a ty ->
    -- | the  controle signal (bool, nat)
    DFVar 'Data ty->
    -- | the data input
    DFVar 'Data ty -> 
    DFApp 'Fun ty

  SelectFun
    -- select :: bool -> A -> A -> A
    :: OutData a ty-> 
    -- | the signal which one to use
    DFVar 'Data ty ->
    -- | the first input
    DFVar 'Data ty ->
    -- | the second input
    DFVar 'Data ty ->
    DFApp 'Fun ty
    
class Function (fun:: FunANF -> Type -> Type) where
  outBindings :: fun a ty -> [TypedBinding ty]
  inBindings :: fun a ty -> [TypedBinding ty]
  funRef :: fun a ty -> QualifiedBinding

-- The fun type is a type that takes a promoted Annotation Type and the 
-- ubiquitouse Host type 'ty' (representing the language (Rust/Python/..) we compile
data Expr (fun :: FunANF -> Type -> Type) (ty :: Type) :: Type where
  Let :: (Show (fun a ty), Function fun) => fun a ty -> Expr fun ty -> Expr fun ty
  -- FIXME this also should probably have a BindingType!
  -- Well it should first of all have a Type type :-/
  Var :: ATypedBinding b ty -> Expr fun ty

----------------------------
-- Accessor functions
----------------------------

-- ToDo: At this point we can/should ensure, that renaming only happens when types are equal
renameABnd :: TypedBinding ty -> ATypedBinding a ty -> ATypedBinding a ty
renameABnd bnew (DataBinding (TBind bnd ty)) = DataBinding bnew
renameABnd bnew (StateBinding (TBind bnd ty)) = StateBinding bnew

outsApp :: App fty ty -> NonEmpty (TypedBinding ty)
outsApp (PureFun out _ _) = unwrapVarTB out :| []
outsApp (StateFun (Nothing, out) _ _ _) = unwrapTB out :| []
outsApp (StateFun (Just stateOut, out) _ _ _) = unwrapTB stateOut :| [unwrapTB out]

outsDFApp :: DFApp fty ty -> [TypedBinding ty]
outsDFApp (PureDFFun out _ _) = NE.toList $ toOutBnds out
outsDFApp (StateDFFun (Nothing, Nothing) _ _ _) = []
outsDFApp (StateDFFun (Nothing, Just out) _ _ _) = NE.toList $ toOutBnds out
outsDFApp (StateDFFun (Just stateOut, Nothing) _ _ _) = NE.toList $ toOutBnds stateOut
outsDFApp (StateDFFun (Just stateOut, Just out) _ _ _) = NE.toList $ toOutBnds stateOut <> toOutBnds out
outsDFApp (RecurFun result recCtrl recurs _ _ _ _) =
  let (x :| xs) = toOutBnds result
   in ( NE.toList $
          (x :| (xs <> join (map (NE.toList . toOutBnds) $ V.toList recurs)))
      )
        ++ maybe [] (NE.toList . toOutBnds) recCtrl
outsDFApp (SMapFun (dOut, collectOut, sizeOut) _) =
  maybe [] (NE.toList . toOutBnds) dOut ++
  maybe [] (NE.toList . toOutBnds) collectOut ++
  maybe [] (NE.toList . toOutBnds) sizeOut
outsDFApp (IfFun (oTrue, oFalse) _) = NE.toList $ toOutBnds oTrue <> toOutBnds oFalse
outsDFApp (CollectFun out _size _lst ) =  (NE.toList . toOutBnds) out 
outsDFApp (SelectFun out _ _ _ ) =  (NE.toList . toOutBnds) out 
outsDFApp (CtrlFun out _sig _data) = (NE.toList . toOutBnds) out 

toOutBnds :: OutData bty ty -> NonEmpty (TypedBinding ty)
toOutBnds (Destruct outs) = sconcat $ NE.map toOutBnds outs
toOutBnds (Dispatch bnds) = NE.map unwrapTB bnds
toOutBnds (Direct bnd) = unwrapTB bnd :| []

insApp :: App fty ty -> [TypedBinding ty]
insApp (PureFun _ _ i) = extractBndsFromInputs $ NE.toList i
insApp (StateFun _ _ (DFVar tbnd) i) = unwrapTB tbnd : extractBndsFromInputs (NE.toList i)

insDFApp :: DFApp fty ty -> [TypedBinding ty]
insDFApp (PureDFFun _ _ i) = extractBndsFromInputs $ NE.toList i
insDFApp (StateDFFun _ _ (DFVar atBnd) i) = (unwrapTB atBnd) : extractBndsFromInputs (NE.toList i)
insDFApp (RecurFun _ _ _ initIns recurs cond result) =
  extractBndsFromInputs (V.toList initIns)
    <> extractBndsFromInputs (V.toList recurs)
    <> extractBndsFromInputs [cond]
    <> extractBndsFromInputs [result]
insDFApp (SMapFun _ dIn) = extractBndsFromInputs [dIn]
insDFApp (IfFun _ dIn) = extractBndsFromInputs [dIn]
insDFApp (CollectFun _ size lst ) = extractBndsFromInputs [size, lst]   
insDFApp (SelectFun _ sign fstIn scndIn ) =   extractBndsFromInputs [sign, fstIn, scndIn ]
insDFApp (CtrlFun _ sig dataIn) = extractBndsFromInputs [sig, dataIn]

extractBndsFromInputs :: [DFVar b ty] -> [TypedBinding ty]
extractBndsFromInputs =
  mapMaybe (\case DFVar atbnd -> Just $ unwrapTB atbnd; _ -> Nothing)


insAndTypesDFApp :: DFApp fty ty -> [TypedBinding ty]
insAndTypesDFApp (PureDFFun _ _ i) = extractBndsAndTypesFromInputs $ NE.toList i
insAndTypesDFApp (StateDFFun _ _ stateIn i) = unwrapVarTB stateIn : extractBndsAndTypesFromInputs (NE.toList i)
insAndTypesDFApp (RecurFun _ _ _ initIns recurs cond result) =
  extractBndsAndTypesFromInputs (V.toList initIns)
    <> extractBndsAndTypesFromInputs (V.toList recurs)
    <> extractBndsAndTypesFromInputs [cond]
    <> extractBndsAndTypesFromInputs [result]
insAndTypesDFApp (SMapFun _ dIn) = extractBndsAndTypesFromInputs [dIn]
insAndTypesDFApp (IfFun _ dIn) = extractBndsAndTypesFromInputs [dIn]
insAndTypesDFApp (CollectFun _ size lst ) = extractBndsAndTypesFromInputs [size, lst]   
insAndTypesDFApp (SelectFun _ sign fstIn scndIn ) =   extractBndsAndTypesFromInputs [sign, fstIn, scndIn ]
insAndTypesDFApp (CtrlFun _ sig dataIn) = extractBndsAndTypesFromInputs [sig, dataIn]

extractBndsAndTypesFromInputs :: [DFVar bty ty] -> [TypedBinding ty]
extractBndsAndTypesFromInputs =
  mapMaybe (\case (DFVar atbnd) -> Just $ unwrapTB atbnd; _ -> Nothing)

fnApp :: App fty ty -> QualifiedBinding
fnApp (PureFun _ (FunRef f _ _) _) = f
fnApp (StateFun _ (FunRef f _ _) _ _) = f

unwrapTB :: ATypedBinding bty ty -> TypedBinding ty
unwrapTB (DataBinding tbnd) = tbnd
unwrapTB (StateBinding tbnd) = tbnd 

unwrapABnd :: ATypedBinding bty ty -> Binding
unwrapABnd (DataBinding tbnd) = asBnd tbnd
unwrapABnd (StateBinding tbnd) = asBnd tbnd 

unwrapVarType :: DFVar b ty -> ArgType ty
unwrapVarType  (DFVar atBnd) = asType . unwrapTB $ atBnd
unwrapVarType  (DFStateVar atBnd) = asType . unwrapTB $ atBnd
unwrapVarType  (DFEnvVar ty _lit) = ty

unwrapVarBnd :: DFVar b ty -> Binding
unwrapVarBnd (DFVar atBnd) = unwrapABnd atBnd
unwrapVarBnd (DFStateVar atBnd) = unwrapABnd atBnd
unwrapVarBnd (DFEnvVar _ _) = error "Tried to unwrap a binding from a literat in DFLang. Please report this error" 

unwrapVarTB :: DFVar b ty -> TypedBinding ty
unwrapVarTB (DFVar atBnd) = unwrapTB atBnd
unwrapVarTB (DFStateVar atBnd) = unwrapTB atBnd
unwrapVarTB (DFEnvVar ty _lit) = error "Tried to unwrap a binding from a literat in DFLang. Please report this error" 

--ToDo: Remove when TypePropagation is gone!
replaceType ::  ATypedBinding bty ty -> ArgType ty ->  ATypedBinding bty ty
replaceType (DataBinding tbnd) newTy = DataBinding (TBind (asBnd tbnd) newTy)
replaceType (StateBinding tbnd) newTy = StateBinding (TBind (asBnd tbnd) newTy)

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
--             outEq :: ATypedBinding b -> ATypedBinding b -> Bool
--             outEq = (==)
--     (StateFun out fn stateIn inp) == (StateFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- So we remove the compile-time safety here and resort entirely to the runtime check:
instance Eq (App ty a) where
  (PureFun out fn inp) == (PureFun out' fn' inp') = unwrapVarBnd out == unwrapVarBnd out' && fn == fn' && inp == inp'
  (StateFun out fn stateIn inp) == (StateFun out' fn' stateIn' inp') =
    out == out' && fn == fn' && stateIn == stateIn' && inp == inp'

-- We should think about these implications a little harder though!
-- deriving instance Lift (App a)

type NormalizedExpr ty = Expr App ty

type NormalizedDFExpr ty = Expr DFApp ty

deriving instance Show (NormalizedExpr ty)

-- deriving instance Eq (NormalizedExpr ty)
--deriving instance Lift (NormalizedExpr ty)
--makeBaseFunctor ''NormalizedExpr
-- deriving instance Lift a => Lift (NormalizedExprF a)

--instance Plated NormalizedExpr where plate = gplate

deriving instance Show (DFApp ty a)

-- deriving instance Eq (DFApp ty a)
-- instance Eq (DFApp a) where
--     (PureDFFun out fn inp) == (PureDFFun out' fn' inp') = out == out' && fn == fn' && inp == inp'
--     (StateDFFun out fn stateIn inp) == (StateDFFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'
-- This does not work for the same reasons as in the case of App a.
-- instance Eq (DFApp a ty) where
--     (PureDFFun out fn inp) == (PureDFFun out' fn' inp') = toOutBnds out == toOutBnds out' && fn == fn' && inp == inp'
--     (StateDFFun out fn stateIn inp) == (StateDFFun out' fn' stateIn' inp') =
--         out == out' && fn == fn' && stateIn == stateIn' && inp == inp'

-- deriving instance Lift (DFApp ty a)

deriving instance Show (NormalizedDFExpr ty)

-- deriving instance Eq NormalizedDFExpr
-- deriving instance Lift NormalizedDFExpr
-- makeBaseFunctor ''NormalizedDFExpr
-- deriving instance Lift a => Lift (NormalizedDFExprF a)
-- instance Plated (NormalizedDFExpr ty) where plate = gplate
-- makeBaseFunctor ''Expr
-- instance Plated (Expr a ty) where plate = gplate

----------------------------------
-- Types and traversals/helpers
----------------------------------

-- TODO
-- data family Expression (a::FunANF -> Type)
-- data instance Expression (a::FunANF -> Type) where
--     NormalizedExpr :: Expression App
--     NormalizedDFExpr :: Expression DFApp

instance Function App where
  outBindings = NE.toList . outsApp
  inBindings = insApp
  funRef = fnApp

instance Function DFApp where
  outBindings = outsDFApp
  inBindings = insDFApp
  funRef f =
    case f of
      (PureDFFun _ (FunRef fr _ _) _) -> fr
      (StateDFFun _ (FunRef fr _ _) _ _) -> fr
      RecurFun {} -> ALangRefs.recurFun
      SMapFun {} -> ALangRefs.smapFun
      IfFun {} -> ALangRefs.ifFun
      CollectFun {} -> ALangRefs.collect
      SelectFun {} -> ALangRefs.select
      CtrlFun {} -> ALangRefs.ctrl

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

universe' :: NormalizedDFExpr ty -> [NormalizedDFExpr ty]
universe' l@(Let _ cont) = l : universe' cont
universe' _ = []

-- This is what I want!
-- paraExpr :: ('Let -> a -> a) -> ('Var -> a) -> NormalizedDFExpr -> a
-- paraExpr = undefined

length :: Expr semTy ty -> V.Nat
length (Let _ cont) = Succ $ length cont
length Var {} = Zero

countBindings :: NormalizedDFExpr ty -> V.Nat
countBindings (Var _ ) = Succ Zero
countBindings (Let app cont) =
  foldl (\acc _ -> Succ acc) (countBindings cont) $ insDFApp app ++ outsDFApp app
 
usedBindings :: NormalizedDFExpr ty -> [TypedBinding ty]
usedBindings (Let app cont) = insDFApp app ++ usedBindings cont
usedBindings (Var result) = [unwrapTB result]


data SubstitutionStrategy = FirstOccurrence | All

-- | Assumes SSA
-- TODO How can we encode this into the type system other than with de Bruijn indexes?
--      I guess we could define a de Bruijn index on the type level that carries a name on the term level.
substitute :: forall ty. SubstitutionStrategy -> (TypedBinding ty, TypedBinding ty) -> NormalizedDFExpr ty -> NormalizedDFExpr ty
substitute strat (from,to) e = evalState (mapFunsM go e) False
  where
    go :: DFApp a ty -> State Bool (DFApp a ty)
    go (PureDFFun o f dIns) = check =<< PureDFFun o f <$> mapM replace dIns
    go (StateDFFun o f sIn dIns) = check =<< StateDFFun o f sIn <$> mapM replace dIns
    go r@RecurFun{} = return r -- we do not replace these because RecurFun has variables that not yet defined.
                               -- we need a better representation for these.
    go (SMapFun o i) = check =<< SMapFun o <$> replace i
    go (IfFun o i) = check =<< IfFun o <$> replace i
    go (CollectFun o fstIn scdIn) = check =<< CollectFun o <$> replace fstIn <*> replace scdIn
    go (CtrlFun o ctrlIn dataIn) = check =<< CtrlFun o <$> replace ctrlIn <*> replace dataIn
    go (SelectFun o sign fstIn scdIn) = check =<< SelectFun o <$> replace sign <*> replace fstIn <*> replace scdIn
    replace :: DFVar t ty -> State Bool (DFVar t ty)
    replace d@(DFVar atbnd) | unwrapTB atbnd == from = do
                              s <- get
                              case (strat, s) of
                                (FirstOccurrence, True)  -> return d
                                (FirstOccurrence, False) -> DFVar (renameABnd to  atbnd) <$ put True
                                _ -> return $ DFVar (renameABnd to atbnd)
    replace d = return d

    check :: DFApp a ty -> State Bool (DFApp a ty)
    check app = checkDefined app >> return app

    checkDefined :: DFApp a ty -> State Bool ()
    checkDefined f =
      if HS.member from $ HS.fromList $ outBindings f
      then put True
      else return ()
