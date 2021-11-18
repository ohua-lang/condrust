{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F
import qualified Ohua.Backend.Types as B
import qualified Ohua.Core.Compile.Configuration as CConfig
import Ohua.Integration.Config (Config(..))
import Ohua.Integration.Lang
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Backend.Passes (passes)
import Ohua.Integration.Rust.Architecture.SharedMemory ()
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform ()
import Ohua.Integration.Rust.Architecture.M3 ()
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()


type FileExtension = Text

type FullIntegration lang arch =
  ( F.Integration (Language lang)
  , B.Integration (Language lang)
  , F.NS (Language lang) ~ B.NS (Language lang)
  , F.Type (Language lang) ~ B.Type (Language lang)
  , F.AlgoSrc (Language lang) ~ B.AlgoSrc (Language lang)
  , B.Architecture (Architectures arch)
  , B.Lang (Architectures arch) ~ Language lang
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

runIntegration :: CompM m
                => Text
                -> Config
                -> Compiler m a
                -> m a
runIntegration ext (Config arch options) comp =
  case ext of
    ".rs" -> apply (loadIntegration SRust arch) comp
    _ -> throwError $ "No language integration defined for files with extension '" <> ext <> "'"
  where
    loadIntegration SRust SharedMemory = I SRust SSharedMemory $ Just $ passes options
    loadIntegration SRust M3  = I SRust SM3 Nothing
