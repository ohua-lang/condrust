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

-- | Numerical reference to a spliced expression from the host environment.
newtype HostExpr = HostExpr
    { unwrapHostExpr :: Int
    } deriving (Eq, Ord, Generic, Show, Lift, Hashable, NFData)

data ArgType ty = TypeVar | Type ty deriving (Lift, Generic)
data FunType ty where
     Untyped :: FunType ty
     FunType :: [ArgType ty] -> FunType ty 
     STFunType :: ArgType ty -> [ArgType ty] -> FunType ty

data FunRef ty where
    FunRef :: QualifiedBinding -> Maybe FnId -> FunType ty -> FunRef ty

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

instance EqNoType (ArgType ty) where
    TypeVar ~= TypeVar = True
    Type _ ~= Type _ = True -- skipping to type info here!
    _ ~= _ = False

instance Eq (ArgType ty) where
    (==) = (~=)

instance ShowNoType (ArgType ty) where
    showNoType TypeVar = "TypeVar"
    showNoType (Type _) = "Type _"

instance Show (ArgType ty) where
    show = T.unpack . showNoType

instance Hashable (ArgType ty) where
    hashWithSalt s TypeVar = s
    hashWithSalt s (Type _) = s

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
type instance SourceType HostExpr = Int

instance UnsafeMake FnId where unsafeMake = FnId
instance UnsafeMake Binding where unsafeMake = Binding
instance UnsafeMake HostExpr where unsafeMake = HostExpr
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

instance Make HostExpr where
    make i
        | i < 0 = throwError "HostExpr cannot be < 0"
        | otherwise = pure $ unsafeMake i


instance Unwrap Binding where
    unwrap (Binding b) = b

instance Unwrap NSRef where
    unwrap (NSRef l) = l

instance Unwrap FnId where
    unwrap (FnId i) = i

instance Unwrap HostExpr where
    unwrap (HostExpr i) = i

instance Plated FnId where plate = gplate
instance Plated QualifiedBinding where plate = gplate
instance Plated HostExpr where plate = gplate
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

-- Only exists to allow literal integers to be interpreted as host expressions
instance Num HostExpr where
    fromInteger = makeThrow . fromInteger
    (+) = intentionally_not_implemented
    (-) = intentionally_not_implemented
    (*) = intentionally_not_implemented
    abs = intentionally_not_implemented
    signum = intentionally_not_implemented

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
