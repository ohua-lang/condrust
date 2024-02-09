module Ohua.Commons.Types.Classes where

import Universum
import qualified Data.Text as T


-- TODO move these type classes and their instances to a separate module

class ShowNoType a where
    showNoType :: a -> Text

class EqNoType a where
    (~=) :: a -> a -> Bool

infixl 5 ~=

instance ShowNoType Text where
    showNoType t = t

instance (ShowNoType a) => ShowNoType (NonEmpty a) where
    showNoType xs = T.intercalate " " $ toList $ map showNoType xs

instance (ShowNoType a) => ShowNoType [a] where
    showNoType xs = T.intercalate " " $ toList $ map showNoType xs

instance (EqNoType a, EqNoType b) => EqNoType (a,b) where
    (a,b) ~= (a',b') = a ~= a' && b ~= b'

instance (EqNoType a) => EqNoType [a] where
    as ~= as' = all (uncurry (~=)) $ zip as as'

instance (EqNoType a) => EqNoType (NonEmpty a) where
    as ~= as' = toList as ~= toList as'

instance (EqNoType a, EqNoType b) => EqNoType (Either a b) where
    Left a ~= Left a' = a ~= a' 
    Right b ~= Right b' = b ~= b'
    _ ~= _ = False

instance (EqNoType a) => EqNoType (Maybe a) where
    Just a ~= Just a' = a ~= a'
    _ ~= _ = False
