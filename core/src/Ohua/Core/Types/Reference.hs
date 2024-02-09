{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveLift, MultiWayIf #-}
module Ohua.Core.Types.Reference where

import Ohua.Prelude

import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)


-- | Utility type for parsing. Denotes a binding which may or may not
-- be qualified.
data SomeBinding
    = Unqual Binding
    | Qual QualifiedBinding
    deriving (Eq, Show, Lift, Generic)


--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

instance NFData SomeBinding
instance Hashable SomeBinding

instance IsString SomeBinding where
    fromString = either Unqual Qual . either error id . symbolFromString . toText
