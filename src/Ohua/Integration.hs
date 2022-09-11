{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F
import qualified Ohua.Backend.Types as B
import qualified Ohua.Core.Compile.Configuration as CConfig
import Ohua.Integration.Config (Config(..))
import Ohua.Integration.Lang
import Ohua.Integration.Architecture
import qualified Ohua.Integration.Rust.Backend.Passes as RustPasses
import Ohua.Integration.Rust.Architecture.SharedMemory ()
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform ()
import Ohua.Integration.Rust.Architecture.M3 ()
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()

import qualified Ohua.Integration.Python.Backend.Passes as PyPasses
import Ohua.Integration.Python.MultiProcessing ()
import Ohua.Integration.Python.Transform ()
import Ohua.Integration.Python.Frontend ()
import Ohua.Integration.Python.NewBackend ()

import qualified Data.Text as T


type FileExtension = Text

type FullIntegration lang arch =
  ( F.Integration (Language lang)
  , B.Integration (Language lang)
  , F.HostModule (Language lang) ~ B.HostModule (Language lang)
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
runIntegration ext (Config arch options) comp = do
  integration <- case ext of
    ".rs" -> case arch of
               SharedMemory -> return $ I SRust SSharedMemory $ Just $ RustPasses.passes options
               M3 -> return $ I SRust SM3 Nothing
    ".py" -> case arch of
               MultiProcessing-> return $ I SPython SMultiProc $ Just $ PyPasses.passes options
    _ -> throwError $ "No language integration defined for files with extension '" <> ext <> "'"
  apply integration comp
