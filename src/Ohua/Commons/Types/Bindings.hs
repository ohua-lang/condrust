{-# LANGUAGE
    TemplateHaskell
    , GeneralizedNewtypeDeriving
    , DeriveLift
    , MultiWayIf
    , ConstraintKinds
    , QuantifiedConstraints
    , UndecidableInstances
    , DataKinds
    , TypeFamilies
    , StandaloneDeriving
    , ScopedTypeVariables
    , TypeApplications
#-}

module Ohua.Commons.Types.Bindings where

import Universum hiding (fromList)

import GHC.Generics
import Control.Lens.TH
import Control.Lens.Plated
import Instances.TH.Lift ()
import Data.Text.Prettyprint.Doc (Pretty(..))
import GHC.Exts (IsList(..))
import qualified Data.Text as T
import Control.Monad.Error.Class (MonadError, throwError)
import Language.Haskell.TH.Syntax as TH (Lift(..))
import Language.Haskell.TH.Lift (makeLift)

import System.FilePath.Posix (addExtension)
import System.FilePath as Path (joinPath)


import Ohua.Commons.Util
import Ohua.Commons.LensClasses
import Ohua.Commons.Types.Error
import Ohua.Commons.Types.Make
import Ohua.Commons.Types.Classes
import Ohua.Commons.Types.Unit (Unit)

import qualified Text.Show


--------------------------------------------------------------
--               Representation of Variables
--------------------------------------------------------------

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

--------------------------------------------------------------
--             Representation of Functions
--------------------------------------------------------------


-- | The numeric id of a function call site
newtype FnId =
    FnId Int
    deriving (Eq, Ord, Generic, Enum, Num, NFData, Hashable, Show, Lift)

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

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
