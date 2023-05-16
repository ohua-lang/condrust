{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveLift, MultiWayIf #-}
module Ohua.Types.Reference where

import Universum

import Control.Lens.Plated
import Control.Lens.TH
import Control.Monad.Error.Class (MonadError, throwError)
import qualified Data.Text as T
import GHC.Exts (IsList(..))
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax as TH (Lift(..))
import Ohua.LensClasses

import System.FilePath.Posix (addExtension)
import System.FilePath as Path (joinPath)
import Ohua.Util
import Ohua.Types.Error
import Ohua.Types.Make
import Ohua.Types.Classes
import Ohua.Types.Unit (Unit)

import qualified Text.Show


-- | A binding name
newtype Binding =
    Binding Text
    deriving (Eq, Hashable, Generic, Ord, Monoid, Semigroup, NFData, Show, Lift)

-- | Hierarchical reference to a namespace
newtype NSRef =
    NSRef [Binding]
    deriving (Eq, Generic, NFData, Ord, Show, Lift)

-- | A qualified binding. References a particular bound value inside a
-- namespace.
declareFields [d|
    data QualifiedBinding = QualifiedBinding
        { qualifiedBindingNamespace :: NSRef
        , qualifiedBindingName      :: Binding
        } deriving (Eq, Generic, Ord, Show, Lift)
  |]

-- | The numeric id of a function call site
newtype FnId =
    FnId Int
    deriving (Eq, Ord, Generic, Enum, Num, NFData, Hashable, Show, Lift)


-- | Internal type representations. While Type and TupleTy capture types from the
--   host language, TypeVar, TypeNat and TypeBool are used internaly to construct nodes
--   They must be mapped to the according types of the host language in the backend or, in 
--   in case of TypeVar need to be eliminated for Backends requiring typed channels.
data VarType ty 
    = TypeVar 
    | TypeNat 
    | TypeBool 
    | TypeUnit 
    | TypeString 
    | TypeList (VarType ty) 
    | Type ty 
    | TupleTy (NonEmpty (VarType ty)) 
    -- REMINDER: Can't derive Lift for Unit, therefor not for FunType and therefor I can't have FunType here for now
    --           Find a way to fix this
    | TypeFunction [VarType ty] (VarType ty)
    deriving (Lift, Generic)

-- ToDo: This is just a helper until we get types of control nodes right
controlSignalType :: VarType ty
controlSignalType = TupleTy $ TypeBool:| [TypeNat]

-- No More untyped functions and functions have a return type
data FunTypeNew ty where
     FunTypeNew :: Either Unit (NonEmpty (VarType ty)) -> VarType ty -> FunTypeNew ty
     STFunTypeNew :: VarType ty -> Either Unit (NonEmpty (VarType ty)) -> VarType ty -> FunTypeNew ty

data FunType ty where
     Untyped :: FunType ty
     FunType :: Either Unit (NonEmpty (VarType ty)) -> FunType ty
     STFunType :: VarType ty -> Either Unit (NonEmpty (VarType ty)) -> FunType ty


data FunRef ty where
    FunRef :: QualifiedBinding -> Maybe FnId -> FunType ty -> FunRef ty

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

instance EqNoType (VarType ty) where
    TypeVar ~= TypeVar = True
    TypeNat ~= TypeNat = True
    TypeBool ~= TypeBool = True
    TypeUnit ~= TypeUnit = True
    TypeString ~= TypeString = True
    Type _ ~= Type _ = True -- skipping to type info here!
    (TupleTy ts) ~= (TupleTy ts') = ts == ts' -- tuns into ~=, see instance below
    (TypeList inner1) ~= (TypeList inner2) = inner1 == inner2
    _ ~= _ = False

instance Eq (VarType ty) where
    (==) = (~=)

instance ShowNoType (VarType ty) where
    showNoType TypeVar = "TypeVar"
    showNoType TypeNat = "Internal nat"
    showNoType TypeBool = "Internal bool"
    showNoType TypeUnit = "Internal Unit"
    -- Is it internal though?
    showNoType TypeString = "Internal String"
    showNoType (TypeList ts) = "Internal List [" <> showNoType ts <> "]"
    showNoType (Type _) = "HostType _"
    showNoType (TupleTy ts) = "(" <>  foldl (\b a -> show a <> ", " <> b) ")" ts


instance Show (VarType ty) where
    show = T.unpack . showNoType

instance Hashable (VarType ty) where
    hashWithSalt s TypeVar = s
    hashWithSalt s TypeNat = s
    hashWithSalt s TypeBool = s
    hashWithSalt s TypeUnit = s
    hashWithSalt s TypeString = s
    hashWithSalt s (TypeList ts) = s
    hashWithSalt s (Type _) = s
    hashWithSalt s (TupleTy _) = s

deriving instance Show (FunType ty)
deriving instance Eq (FunType ty)
deriving instance Generic (FunType ty)
instance Hashable (FunType ty)

deriving instance Show (FunRef ty)
deriving instance Eq (FunRef ty)
deriving instance Generic (FunRef ty)
instance Hashable (FunRef ty)

instance NFData QualifiedBinding

instance Hashable NSRef where
    hashWithSalt salt = hashWithSalt salt . unwrap
    {-# INLINE hashWithSalt #-}
instance Hashable QualifiedBinding

type instance SourceType FnId = Int
type instance SourceType Binding = Text
type instance SourceType NSRef = [Binding]

instance UnsafeMake FnId where unsafeMake = FnId
instance UnsafeMake Binding where unsafeMake = Binding
instance UnsafeMake NSRef where
    unsafeMake = NSRef



instance Make FnId where
    make i
        | i < 0 =
            throwError $
            "Function id must be larger than 0, was " <> show i
        | otherwise = pure $ unsafeMake i

instance Make Binding where
    make "" = throwError "Binding cannot be empty"
    make s = pure $ unsafeMake s

instance Make NSRef where
    make = pure . unsafeMake

instance Unwrap Binding where
    unwrap (Binding b) = b

instance Unwrap NSRef where
    unwrap (NSRef l) = l

instance Unwrap FnId where
    unwrap (FnId i) = i

instance Plated FnId where plate = gplate
instance Plated QualifiedBinding where plate = gplate
instance Plated Binding where plate = gplate
instance Plated NSRef where plate = gplate


instance IsString Binding where
    fromString = makeThrow . toText

instance IsString QualifiedBinding where
    fromString s = case either error id $ symbolFromString $ toText s of
        Left b -> error $ fromString $ "Encountered unqualified binding: " ++ show b
        Right q -> q

instance IsList NSRef where
    type Item NSRef = Binding
    fromList = makeThrow . fromList
    toList = GHC.Exts.toList . unwrap

-- | Attempt to parse a string into either a binding or a
-- qualified binding.  Assumes a form "name.space/value" for qualified
-- bindings.
symbolFromString :: MonadError Error m => Text -> m (Either Binding QualifiedBinding)
symbolFromString s
    | T.null s = throwError "Symbols cannot be empty"
    | otherwise =
        case T.break (== '/') s of
            (symNs, slashName)
                | T.null symNs ->
                    throwError $
                    "An unqualified name cannot start with a '/': " <> show s
                | T.null slashName -> Left <$> make symNs
                | Just ('/', symName) <- T.uncons slashName ->
                    if | isJust ((== '/') `T.find` symName) ->
                           throwError $
                           "Too many '/' delimiters found in the binding " <>
                           show s
                       | T.null symName ->
                           throwError $
                           "Name cannot be empty in the binding " <> show s
                       | otherwise ->
                           do nspace <- make =<< mapM make (T.split (== '.') symNs)
                              bnd <- make symName
                              pure $ Right $ QualifiedBinding nspace bnd
            _ ->
                throwError $
                "Leading slash expected after `break` in the binding " <> show s


nsToFilePath :: NSRef -> FilePath
nsToFilePath = joinPath . map (T.unpack . unwrap) . unwrap 

toFilePath :: (NSRef, Text) -> FilePath
toFilePath (nsRef, suffix) = addExtension (nsToFilePath nsRef) $ T.unpack suffix
