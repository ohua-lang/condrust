{-# LANGUAGE TemplateHaskell #-}
module Ohua.Core.Types.Environment where

import Ohua.Prelude

import Control.Lens.TH
import qualified Data.Vector as V

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
    data OhuaState envExpr = OhuaState
        { nameGenerator :: !NameGenerator
        , idCounter :: !FnId
        , envExpressions :: !(V.Vector envExpr)
        }
  |]


type instance SourceType (OhuaState envExpr) =
     (NameGenerator, FnId, V.Vector envExpr)


instance Make (OhuaState envExpr) where
    make (ng, fnid, exprs) = pure $ OhuaState ng fnid exprs


instance Default Options where
    def =
        Options
            Nothing
            Nothing
            False -- for now we always disable this option
            (const (Don'tDump, False))

instance Default Environment where
    def = Environment def
