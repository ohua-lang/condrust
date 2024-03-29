module Ohua.Commons.Types.Make where

import Universum

import Control.Monad.Error.Class (MonadError)

import Ohua.Commons.Types.Error

type family SourceType t


-- | Though it may seem innocuous, this class, as well as 'Unwrap',
-- are instrumental to the types defined in these modules. 'Make'
-- provides a way for someone needing to use types defined in this
-- module to safely instantiate them. 'Unwrap' provides a way of
-- inspecting the raw values stored underneath. Though caution is
-- advised with regards to the stability of that representation.
class Make t where
    -- | Safely construct a value of type @t@ from its source type
    -- potentially reporting an error.
    make :: MonadError Error m => SourceType t -> m t

-- | Same as 'make' but instead throws an exception
makeThrow :: Make t => SourceType t -> t
makeThrow = either error id . make

-- | Convert a value @t@ back to its source type @t@.
class Unwrap t where
    unwrap :: t -> SourceType t

-- | Unsafe version of 'make'. Constructs the type skipping the checks.
class UnsafeMake t where
    unsafeMake :: SourceType t -> t


unwrapped :: (Make t, Unwrap s) => Lens s t (SourceType s) (SourceType t)
unwrapped f s = makeThrow <$> f (unwrap s)


class Embed into from where
    embedE :: from -> into