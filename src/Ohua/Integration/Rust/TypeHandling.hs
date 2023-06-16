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
rustUnitReturn = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "()" Nothing ()] ()) ()

rustBool :: Ty ()
rustBool = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()

rustI32 :: Ty ()
rustI32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) () 

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
                        let fName = createFunRef ident
                            (argTys, retTy) = getTypes decl
                            fType =  FunType argTys retTy
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

        createFunRef :: Ident -> QualifiedBinding
        createFunRef =
            QualifiedBinding (filePathToNsRef srcFile) .
            toBinding
        
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
            _ -> Just . (createFunRef ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromImplItem _ _ = return Nothing

        extractFromTraitItem :: (ErrAndLogM m, Show a) => Ty a -> TraitItem a -> m (Maybe (QualifiedBinding, FunType RustVarType ))
        extractFromTraitItem selfType (MethodT _ ident _ (MethodSig _ decl) _ _) =
            Just . (createFunRef ident, ) <$> extractFunType (extractFirstArg selfType) decl
        extractFromTraitItem _ _ = return Nothing

        extractFirstArg :: Ty a -> Arg a -> [VarType RustVarType] -> VarType RustVarType -> m (FunType RustVarType)
        extractFirstArg selfType fstArg args retTy =
            let funType x0 = case x0 of
                            (Type (HostType Self{})) -> STFunType x0 args retTy
                            _ -> FunType (x0 : args) retTy
            in funType <$> convertImplArg selfType fstArg 


maxType :: VarType RustVarType -> VarType RustVarType -> VarType RustVarType
maxType (Type (HostType Unknown)) (Type (HostType Unknown)) = (Type (HostType Unknown))
maxType (Type (HostType Unknown)) t2 = t2
maxType t1 (Type (HostType Unknown)) = t1
maxType t1 t2 | t1 == t2 = t1
maxType t1 t2 = error $ "Typing error. Comparing types " <> show t1 <> " and " <> show t2