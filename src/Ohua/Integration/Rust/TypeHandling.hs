{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Rust.TypeHandling where

import Ohua.Prelude

import Ohua.Integration.Rust.Util

import Ohua.Types.Unit (Unit)

import Language.Rust.Syntax as Rust hiding (Normal, Type)
import Language.Rust.Data.Ident (Ident)
import Language.Rust.Parser (Span)
import Language.Rust.Pretty as RustP

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.Prettyprint.Doc as Doc (Pretty(..))
import Data.List.NonEmpty hiding (map)


data Module = Module FilePath (SourceFile Span)


-- We currently have the problem, that during lowering the rust code it is ok for some things to be not properly typed i.e.
-- we first translate the algo, extracting the type info from annotations, next we extracttype info from imported functions and last we
-- (will) try to merge those infos to find out if there's anything elft untyped. 
-- We used to represent unknown types by 'TypeVar'. However we don't want things INSIDE the compiler to be of unknown type so we need to move this 
-- representation of 'unknown type' into the realm of the rust representation (it's really just Rust because in Python we don't have that problem)
-- The most accurate solution would be to have two Rust type representations one including 'untyped' and on without that we would pass further down the compiler to
-- really rule out untyped stuff in the compiler. But this would require refactoring the definitions (types) of Integration, passes and what not. So to 
-- keep it simple for know and first finish the task of eliminating 'TypeVar', we'll include an 'untyped' in the Rust type representation. 
data RustVarType = Self (Ty ()) (Maybe (Lifetime ())) Mutability | Normal (Ty ()) | Unknown deriving (Show, Eq)
type RustHostType = HostType RustVarType
type FunTypes = HM.HashMap QualifiedBinding (FunType RustVarType)


type VarTypeContext = HM.HashMap Binding (VarType RustVarType)
type TypeContextM m = (Monad m, MonadState VarTypeContext m)


instance Doc.Pretty RustVarType where
    pretty (Self ty lT  mut) = RustP.pretty' ty
    pretty (Normal ty) = RustP.pretty' ty
    pretty Unknown = "Unknown" 

rustUnitReturn :: Ty ()
rustUnitReturn = Rust.TupTy [] () -- Nothing (Rust.Path False [Rust.PathSegment "()" Nothing ()] ()) ()

rustBool :: Ty ()
rustBool = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()

rustI32 :: Ty ()
rustI32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) () 

hostReturnSelf :: VarType RustVarType
hostReturnSelf = Type $ HostType $ Normal $ PathTy Nothing (Path False [PathSegment "Self" Nothing ()] ()) () 

rustInfer :: Ty ()
rustInfer = Infer ()

asHostNormal:: Ty a -> VarType RustVarType
asHostNormal ty = Type $ HostType $ Normal (deSpan ty)

asHostSelf:: Ty a -> (Maybe (Lifetime a)) -> Mutability-> VarType RustVarType
asHostSelf ty lt mut = Type $ HostType $ Self (deSpan ty) (map deSpan lt) mut

typeUnknown:: VarType RustVarType
typeUnknown = Type $ HostType Unknown

isUnknown :: VarType RustVarType -> Bool
isUnknown ty = ty == (Type (HostType Unknown))


-- REMINDER: Commented out because we don't scan the imported libraries for type extraction any more

-- | Load the given file as AST, pattern match on the content and collect
--   the types of all defined functions (fn, impl or inside trait) into a 
--   Hashmap @FunTypes@ mapping function bindings to their types

extractFromFile :: ErrAndLogM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (loadRustFile srcFile)

extract :: forall m a. (ErrAndLogM m, Show a) => FilePath -> SourceFile a -> m (HM.HashMap QualifiedBinding (FunType RustVarType))
extract srcFile (SourceFile _ _ items) = HM.fromList <$> extractTypes items
    where
        extractTypes :: (ErrAndLogM m, Show a) => [Item a] -> m [(QualifiedBinding, FunType RustVarType)]
        extractTypes items = 
            catMaybes . concat <$>
            mapM
                (\case
                    (Fn _ _ ident decl _ _ _ _ ) -> do 
                        let fName = createFunRef ident Nothing
                            (argTys, retTy) = getTypes decl
                            fType =  FunType argTys retTy
                        -- traceShowM $ "Normal function: " <> show fName <> ": " <> show fType
                        return (Just (fName, fType): [])
                    (Impl _ _ _ _ _ _ _ selfType items _) -> 
                        mapM (extractFromImplItem selfType) items
                    (Trait _ _ ident _ _ _ _ items span) -> 
                        mapM (extractFromTraitItem (toTraitType ident span)) items
                    _ -> return [])
                items

        toTraitType :: Show a => Ident -> a -> Ty a
        toTraitType ident span =
            TraitObject
                (TraitBound
                    (PolyTraitRef
                        []
                        (TraitRef $ Path False [PathSegment ident Nothing span] span)
                        span)
                    None
                    span
                :| [])
                span

        createFunRef :: Ident -> Maybe (Ty a)-> QualifiedBinding
        createFunRef funIdent maybeStructType =
            case maybeStructType of
                -- The namespace qualified bining of impl and trait functions needs to include the struct they are implemented for
                -- ToDo: We still have to solve the problem of multiple traits for the same struct defining equally named functions
                Just ty -> QualifiedBinding (makeThrow $ filePathToList srcFile ++ [show $ RustP.pretty' (deSpan ty)]) $ toBinding funIdent
                Nothing -> QualifiedBinding (filePathToNsRef srcFile) $ toBinding funIdent

        getTypes :: Show a => FnDecl a -> ([VarType RustVarType], VarType RustVarType)
        getTypes f@(FnDecl _ _ True _) = error $ "Currently, we do not support variadic arguments." <> show f
        getTypes (FnDecl args retType _ _) =  (map toVarType args, fromMaybeRet retType)

        fromMaybeRet:: Maybe (Ty a) -> VarType RustVarType
        fromMaybeRet (Just retTy) = asHostNormal retTy
        fromMaybeRet Nothing = asHostNormal rustUnitReturn

        extractFunType :: (ErrAndLogM m, Show a) =>
            (Arg a -> [VarType RustVarType] -> VarType RustVarType -> m (FunType RustVarType)) ->
            FnDecl a ->
            m (FunType RustVarType)
        extractFunType _ f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
        extractFunType f (FnDecl args retType _ _) =
            case args of
                [] -> return $ FunType [] (fromMaybeRet retType)
                (fstArg: args) -> f fstArg  (map toVarType args) (fromMaybeRet retType)

        convertImplArg :: (ErrAndLogM m, Show a) => Ty a -> Arg a -> m (VarType RustVarType)
        convertImplArg selfType (SelfValue _ mut _) = return $ asHostSelf selfType Nothing mut
        convertImplArg selfType (SelfRegion _ lifeTime mut _) = return $ asHostSelf selfType lifeTime mut -- Type $ Self (void selfType) (void <$>lifeTime) mut
        convertImplArg selfType (SelfExplicit _ _ty mut _) = return $ asHostSelf selfType Nothing mut
        convertImplArg _ a = return $ toVarType a

        toVarType :: Show a => Arg a -> VarType RustVarType
        toVarType (Arg _ _ typ _) = asHostNormal typ
        toVarType a = error $ "Please report: The impossible happened at argument: " <> show a

        extractFromImplItem :: (ErrAndLogM m, Show a) => Ty a -> ImplItem a -> m (Maybe (QualifiedBinding, FunType RustVarType))
        extractFromImplItem selfType (MethodI _ _ _ ident _ (MethodSig _ decl) _ _) =
          case decl of
            FnDecl [] _ _ _ -> return Nothing
            _ -> Just . (createFunRef ident (Just selfType), ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromImplItem _ _ = return Nothing

        extractFromTraitItem :: (ErrAndLogM m, Show a) => Ty a -> TraitItem a -> m (Maybe (QualifiedBinding, FunType RustVarType ))
        extractFromTraitItem selfType (MethodT _ ident _ (MethodSig _ decl) _ _) =
            Just . (createFunRef ident (Just selfType), ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromTraitItem _ _ = return Nothing

        extractFirstArg :: Ty a -> Arg a -> [VarType RustVarType] -> VarType RustVarType -> m (FunType RustVarType)
        extractFirstArg selfType fstArg args retTy =
            -- Replace a return type Self with the actual name of the struct 
            let actualReturnType = if retTy == hostReturnSelf then asHostNormal selfType else retTy
            
                funType x0 = case x0 of
                            (Type (HostType Self{})) -> STFunType x0 args actualReturnType
                            _ -> FunType (x0 : args) actualReturnType
            in funType <$> convertImplArg selfType fstArg 


maxType :: VarType RustVarType -> VarType RustVarType -> VarType RustVarType
-- ^ Unknown == Unknown
maxType (Type (HostType Unknown)) (Type (HostType Unknown)) = (Type (HostType Unknown))
-- ^ Any type > Unknown
maxType (Type (HostType Unknown)) t2 = t2
maxType t1 (Type (HostType Unknown)) = t1
-- ^ equal types -> take the first
maxType t1 t2 | t1 == t2 = t1

-- ^ unequal host types -> for know thats an error, but actually we need to resort to Rust here e.g Self vs ActualType => ActuaType
maxType (Type t1) (Type t2) = error $ "Typing error. Comparing types " <> show t1 <> " and " <> show t2

-- Problem: For an arbitrary host ty, we can not tell if it should be coercible to internal Types we assign to literals e.g. NumericLit/Boollit etc.
-- What we need are HostLits, with a String representation and a Host Type. So far we don't have that andI'll hack arround the type system by always giving 
-- precedence to the host type. 
maxType t@(Type (HostType ty)) someInternalType = t
maxType someInternalType t@(Type (HostType ty)) = t

maxType t1 t2 | t1 /= t2 = error $ "Typing error. Comparing types " <> show t1 <> " and " <> show t2