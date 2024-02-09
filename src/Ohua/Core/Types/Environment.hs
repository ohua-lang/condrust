{-# LANGUAGE TemplateHaskell #-}
module Ohua.Core.Types.Environment where

import Ohua.Commons.Prelude

import Control.Lens.TH

import Ohua.Core.Types.Stage


declareLenses [d|
    data Options = Options
        { callEnvExpr :: !(Maybe QualifiedBinding)
        , callLocalFunction :: !(Maybe QualifiedBinding)
        , transformRecursiveFunctions :: Bool
        , stageHandling :: StageHandling
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


type instance SourceType OhuaState =
     (NameGenerator, FnId)


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
