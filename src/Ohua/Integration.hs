{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F
import qualified Ohua.Backend.Types as B
import qualified Ohua.Core.Compile.Configuration as CConfig
import Ohua.Integration.Lang
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Architecture.SharedMemory ()
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform ()
import Ohua.Integration.Transform.DataPar (dataPar)
import Ohua.Integration.Rust.Architecture.M3 ()
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()


type FileExtension = Text
type ArchId = Text
type Description = Text

type FullIntegration lang arch =
  ( F.Integration (Language lang)
  , B.Integration (Language lang)
  , F.NS (Language lang) ~ B.NS (Language lang)
  , F.Type (Language lang) ~ B.Type (Language lang)
  , F.AlgoSrc (Language lang) ~ B.AlgoSrc (Language lang)
  , B.Architecture (Architectures arch)
  , B.Lang (Architectures arch) ~ (Language lang)
  , B.Transform (Architectures arch)
  )

type Compiler m a = (forall lang arch.
                     FullIntegration lang arch
                    => Maybe CConfig.CustomPasses
                    -> Language lang
                    -> Architectures arch
                    -> m a)

data Integration =
    forall (lang::Lang) (arch::Arch).
    FullIntegration lang arch =>
    I (Language lang) (Architectures arch) (Maybe CConfig.CustomPasses)

class Apply integration where
    apply :: CompM m
        => integration
        -> Compiler m a
        -> m a

instance Apply Integration where
    apply (I lang arch config) comp = comp config lang arch

definedIntegrations :: [(FileExtension, ArchId, Description, Integration)]
definedIntegrations =
    [ (".rs", "sm", "Rust integration"
      , I SRust SSharedMemory Nothing
      --, I SRust SSharedMemory $ Just dataPar
      )
    , (".rs", "m3", "Rust integration", I SRust SM3 Nothing)]

runIntegration :: CompM m
                => Text
                -> Text
                -> Compiler m a
                -> m a
runIntegration ext arch comp
    | Just a <- find (\int -> (int ^. _1 == ext) && (int ^. _2 == arch) ) definedIntegrations
        = apply (a ^. _4) comp
    | otherwise =
        throwError $ "No language integration defined for files with extension '" <> ext <> "'"
