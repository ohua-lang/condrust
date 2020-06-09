{-# LANGUAGE TemplateHaskell #-}
module Ohua.Environment where
-- TODO rename this file!
import Ohua.Prelude

import Control.Lens.TH
import qualified Data.HashSet as HS
import qualified Data.Vector as V



declareLenses [d|
    data Options = Options
        { callEnvExpr :: !(Maybe QualifiedBinding)
        , callLocalFunction :: !(Maybe QualifiedBinding)
        , transformRecursiveFunctions :: Bool
        , stageHandling :: StageHandling
        }
  |]

-- | Stateful name generator
declareLenses [d|
    data NameGenerator = NameGenerator
        { takenNames :: !(HS.HashSet Binding)
        , simpleNameList :: [Binding]
        }
  |]

-- | The read only compiler environment
declareLenses [d|
    newtype Environment = Environment { options :: Options }
  |]

-- | State of the ohua compiler monad.
declareLenses [d|
    data OhuaState = OhuaState
        { nameGenerator :: !NameGenerator
        , idCounter :: !FnId
        }
  |]


type instance SourceType NameGenerator = (HS.HashSet Binding, [Binding])
type instance SourceType OhuaState = (NameGenerator, FnId)


instance UnsafeMake NameGenerator where
    unsafeMake = uncurry NameGenerator


instance Make NameGenerator where
    make = pure . unsafeMake

instance Make OhuaState where
    make (ng, fnid) = pure $ OhuaState ng fnid


instance Default Options where
    def =
        Options
            Nothing
            Nothing
            False -- for now we always disable this option
            (const (Don'tDump, False))

instance Default Environment where
    def = Environment def
