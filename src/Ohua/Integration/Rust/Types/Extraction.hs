{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ohua.Integration.Rust.Types.Extraction where

import Ohua.Commons.Prelude 

import Ohua.Integration.Rust.Util
import Ohua.Integration.Rust.Common.Subset as Sub (TyRef (..), CallRef (..), RustType(..) )
import Ohua.Integration.Rust.Frontend.Convert (convertTy, convertPath)

import Language.Rust.Syntax as Rust hiding (Normal)
import Language.Rust.Data.Ident (Ident(..))
import Language.Rust.Parser (Span, parse, inputStreamFromString)
import Language.Rust.Pretty as RustP

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.Prettyprint.Doc as Doc (Pretty(..))
import qualified Data.List.NonEmpty as NE

import qualified Text.Show

-- ToDo: Should we move this to the Common subset and replace the RustType in Subset?
data Module = Module FilePath (SourceFile Span)

data RustVarType = Self (Ty ()) (Maybe (Lifetime ())) Mutability | Normal (Ty ()) 

instance Show RustVarType where 
    show (Self ty _ _) = show $ RustP.pretty' ty
    show (Normal ty) = show $ RustP.pretty' ty

-- In the type system we need to compare types of states to input types of methods.
-- The former will be something like `Normal Arc` while the later will be `Self Arc`, but we want them to be equal
-- because the self qualifier only occurs in the context of method arguments. 
instance Eq RustVarType where
    (Self tyS _ _) == (Normal tyN)      =  tyS == tyN
    (Normal tyN  ) == (Self tyS _ _ )   =  tyN == tyS
    (Normal tyN  ) == (Normal tyN'  )   =  tyN == tyN'
    (Self tyS _ _ ) == (Self tyS' _ _ ) =  tyS == tyS'

-- ToDo: If we have generics at some point we can hook in more fine grained comparison here
instance Ord RustVarType where
    t1 <= t2 = t1 == t2

type RustHostType = HostType RustVarType
type FunTypesMap = HM.HashMap QualifiedBinding (FunType RustVarType Resolved)


instance Doc.Pretty RustVarType where
    pretty (Self ty _lT _mut) = RustP.pretty' ty
    pretty (Normal ty) = RustP.pretty' ty

instance Pathable RustVarType where
    -- ToDo: Extend patterns if we need something else than PathTy types 
    toPath (Normal rTy) = toPath (RustType rTy) 
    toPath (Self ty _lT _mut) = toPath (RustType ty)
    
    
instance TruthableType RustVarType where
    isHostTruthy (Normal (Rust.PathTy _ (Rust.Path False [Rust.PathSegment "bool" Nothing ()] _) _)) = True
    isHostTruthy _other = False

instance UnTupleType RustVarType where
    unTupleType st@(Self _ty _ _ ) = (st :|[])
    unTupleType nt@(Normal (Rust.TupTy tys _ )) = case tys of
            [] ->  (nt :|[]) -- This is the representation of a Unit type
            (t:ts) ->  NE.map Normal (t:|ts)
    unTupleType nt@(Normal _other) = (nt :|[])

instance ListElementType RustVarType where
    asListElementType (Normal (Rust.PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [TypeArg (itemTy)] _ _ )) _] _) _)) = Just (Normal itemTy) 
    asListElementType _other = Nothing 

instance TellUnitType RustVarType where
    isHostUnit (Normal (Rust.TupTy [] _ )) = True
    isHostUnit _other = False

rustUnitReturn :: Ty ()
rustUnitReturn = Rust.TupTy [] () -- Nothing (Rust.Path False [Rust.PathSegment "()" Nothing ()] ()) ()

rustBool :: Ty ()
rustBool = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment "bool" Nothing ()] ()) ()

rustI32 :: Ty ()
rustI32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()

hostReturnSelf :: OhuaType RustVarType Resolved
hostReturnSelf = HType (HostType $ Normal $ PathTy Nothing (Path False [PathSegment "Self" Nothing ()] ()) ())

rustInfer :: Ty ()
rustInfer = Infer ()

asHostNormal :: Ty a -> OhuaType RustVarType Resolved 
asHostNormal ty = HType (HostType $ Normal (deSpan ty)) 

asHostNormalU :: Ty a -> OhuaType RustVarType Unresolved 
asHostNormalU ty = HType (HostType $ Normal (deSpan ty)) 

asHostSelf :: Ty a -> (Maybe (Lifetime a)) -> Mutability-> OhuaType RustVarType Resolved 
asHostSelf ty lt mut = HType ((HostType $ Self (deSpan ty) (map deSpan lt) mut))


-- | Load the given file as AST, pattern match on the content and collect
--   the types of all defined functions (fn, impl or inside trait) into a
--   Hashmap @FunTypesMap@ mapping function bindings to their types

extractFromFile :: ErrAndLogM m => FilePath -> m FunTypesMap
extractFromFile srcFile = extract srcFile =<< liftIO (loadRustFile srcFile)

extract :: forall m.ErrAndLogM m => FilePath -> SourceFile Span -> m (HM.HashMap QualifiedBinding (FunType RustVarType Resolved))
extract srcFile (SourceFile _ _ items) = HM.fromList <$> extractTypes items
    where
        extractTypes :: [Item Span] -> m [(QualifiedBinding, FunType RustVarType Resolved)]
        extractTypes items' =
            catMaybes . concat <$>
            mapM
                (\case
                    (Fn atts _ ident decl _ _ _ _ ) -> do
                        path <- getPath atts
                        let fName = QualifiedBinding path $ toBinding ident
                        (argTys, retTy) <- getTypes decl
                        let fType =  FunType argTys retTy
                        return (Just (fName, fType): [])
                    (Impl atts _ _ _ _ _ _ selfType items'' _) -> do
                        path <- getPath atts
                        selfTyRef <- convertTy selfType
                        let path' = prependNS path selfTyRef
                        mapM (extractFromImplItem path' selfType) items''
                    (Trait atts _ ident _ _ _ _ items'' span) -> do
                        (NSRef path) <- getPath atts
                        let path' = NSRef $ path ++ [toBinding ident]
                        mapM (extractFromTraitItem path' (toTraitType ident span)) items''
                    _ -> return [])
                items'

        prependNS (NSRef path1) (TyRef (QualifiedBinding (NSRef path2) bnd2) _) =
            NSRef $ path1 ++ path2 ++ [bnd2]

        getPath :: [Attribute Span] -> m NSRef
        getPath atts = do
            externSpecs <- catMaybes <$> mapM checkExternSpec atts
            case externSpecs of
                [] -> return $ makeThrow $ filePathToList srcFile
                [QualifiedBinding (NSRef path) bnd] -> return $ NSRef $ path ++ [bnd]
                xs -> throwError $ "Detected multiple extern_spec definitions. Please only specify one: " <> show xs

        checkExternSpec :: Attribute Span -> m (Maybe QualifiedBinding)
        checkExternSpec (Attribute Outer path ts _) = do
            path' <- convertPath path
            case path' of
                Left "extern_spec" ->
                  case tokenStreamToExpr ts of
                    Right (ParenExpr _ (PathExpr _ Nothing nsPath _) _) -> do
                      nsPath' <- convertPath nsPath
                      case nsPath' of
                        Left bnd -> return $ Just $ QualifiedBinding (makeThrow []) bnd
                        Right (Sub.CallRef qbnd _) -> return $ Just qbnd
                    _ -> return Nothing
                _ -> return Nothing
        checkExternSpec _ = return Nothing

        tokenStreamToExpr = parse @(Rust.Expr Span) . inputStreamFromString . renderStr

        toTraitType :: Ident -> Span -> Ty Span
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

createRef :: NSRef -> Ident -> QualifiedBinding
createRef path funIdent = QualifiedBinding path $ toBinding funIdent

getTypes :: forall m.ErrAndLogM m =>FnDecl Span -> m (Either () (NonEmpty (OhuaType RustVarType Resolved)), OhuaType RustVarType Resolved)
getTypes f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
getTypes (FnDecl [] retType _ _) = return (Left (), fromMaybeRet retType)
getTypes (FnDecl (a:args) retType _ _) = return (Right $ map toVarType (a:|args), fromMaybeRet retType)

fromMaybeRet:: Maybe (Ty Span) -> OhuaType RustVarType Resolved
fromMaybeRet (Just retTy) = asHostNormal retTy
fromMaybeRet Nothing = asHostNormal rustUnitReturn

extractFunType :: forall m.ErrAndLogM m =>
    (Arg Span -> [OhuaType RustVarType Resolved] -> OhuaType RustVarType Resolved -> m (FunType RustVarType Resolved)) ->
    FnDecl Span ->
    m (FunType RustVarType Resolved)
extractFunType _ f@(FnDecl _ _ True _) = throwError $ "Currently, we do not support variadic arguments." <> show f
extractFunType f (FnDecl args retType _ _) =
    case args of
        [] -> return $ FunType (Left ()) (fromMaybeRet retType)
        (fstArg : argumnts) -> f fstArg (map toVarType argumnts) (fromMaybeRet retType)

convertImplArg :: Ty Span -> Arg Span -> OhuaType RustVarType Resolved 
convertImplArg selfType (SelfValue _ mut _) = asHostSelf selfType Nothing mut
convertImplArg selfType (SelfRegion _ lifeTime mut _) = asHostSelf selfType lifeTime mut -- Type $ Self (void selfType) (void <$>lifeTime) mut
convertImplArg selfType (SelfExplicit _ _ty mut _) = asHostSelf selfType Nothing mut
convertImplArg _ (Arg _ _ typ _) = asHostNormal typ

toVarType :: Arg Span -> OhuaType RustVarType Resolved
toVarType (Arg _ _ typ _) = asHostNormal typ
toVarType a = error $ "The impossible happened. Self Type outside of struct of trait discovered: " <> show a

extractFromImplItem :: forall m.ErrAndLogM m => NSRef -> Ty Span -> ImplItem Span -> m (Maybe (QualifiedBinding, FunType RustVarType Resolved))
extractFromImplItem path selfType (MethodI _ _ _ ident _ (MethodSig _ decl) _ _) =
    Just . (createRef path ident, ) <$> extractFunType (extractFirstArg selfType) decl
extractFromImplItem _ _ _ = return Nothing

extractFromTraitItem :: forall m.ErrAndLogM m => NSRef -> Ty Span -> TraitItem Span -> m (Maybe (QualifiedBinding, FunType RustVarType Resolved))
extractFromTraitItem path selfType (MethodT _ ident _ (MethodSig _ decl) _ _) =
    Just . (createRef path ident, ) <$> extractFunType (extractFirstArg selfType) decl
extractFromTraitItem _ _ _ = return Nothing

extractFirstArg :: forall m.ErrAndLogM m => Ty Span -> Arg Span -> [OhuaType RustVarType Resolved] -> OhuaType RustVarType Resolved -> m (FunType RustVarType Resolved)
extractFirstArg selfType fstArg args retTy =
    -- Replace a return type Self with the actual name of the struct
    let actualReturnType = if retTy == hostReturnSelf then asHostNormal selfType else retTy
        argsTys = case args of 
            [] -> Left ()
            (x:xs) -> Right (x:|xs)
        funType x0 = case x0 of
                        (HType (HostType Self{})) -> STFunType x0 argsTys actualReturnType
                        _ -> FunType (Right $ x0 :| args) actualReturnType
    in return $ funType $ convertImplArg selfType fstArg

