{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ohua.Integration where

import Ohua.Prelude hiding (putStrLn)

import qualified Ohua.Frontend.Types as F
import qualified Ohua.Backend.Types as B
import qualified Ohua.Core.Compile.Configuration as CConfig
import Ohua.Integration.Config (Config(..))
import Ohua.Integration.Lang
import Ohua.Integration.Architecture
import qualified Ohua.Integration.Rust.Backend.Passes as RustPasses
import Ohua.Integration.Rust.Architecture.SharedMemory ()
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform ()
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar (liftCollectType)
import Ohua.Integration.Rust.Architecture.M3 ()
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()
-- FIXME: tomland requires a lower verison of base which messes up dependencies -> Find a solution/replacement
-- import Ohua.Integration.Rust.Types.Definition (macro_support)

import qualified Ohua.Integration.Python.Backend.Passes as PyPasses
import Ohua.Integration.Python.MultiProcessing ()
import Ohua.Integration.Python.Transform ()
import Ohua.Integration.Python.Frontend ()
import Ohua.Integration.Python.Backend ()

import qualified Data.Text as T
import Data.Text.IO (putStrLn)


type FileExtension = Text

type FullIntegration lang arch =
  ( F.Integration (Language lang)
  , B.Integration (Language lang)
  , F.HostModule (Language lang) ~ B.HostModule (Language lang)
  , F.Type (Language lang) ~ B.Type (Language lang)
  , F.Type (Language lang) ~ B.LType (Language lang)
  , F.AlgoSrc (Language lang) ~ B.AlgoSrc (Language lang)
  , B.Architecture (Architectures arch)
  , B.Lang (Architectures arch) ~ Language lang
  , B.Transform (Architectures arch)
  )

type Compiler m a = (forall lang arch.
                     FullIntegration lang arch
                    => Maybe (CConfig.CustomPasses (B.Type (Language lang)))
                    -> Language lang
                    -> Architectures arch
                    -> m a)

data Integration =
    forall (lang::Lang) (arch::Arch).
    FullIntegration lang arch =>
    Integration (Language lang)
        (Architectures arch)
        (Maybe (CConfig.CustomPasses (B.Type (Language lang))))

class Apply integration where
    apply :: ErrAndLogM m
        => integration
        -> Compiler m a
        -> m a

instance Apply Integration where
    apply (Integration lang arch customCorePasses) comp = comp customCorePasses lang arch

runIntegration :: ErrAndLogM m
                => Text
                -> Config
                -> Compiler m a
                -> m a
runIntegration ext (Config arch options) comp = do
  integration <- case ext of
    ".rs" -> case arch of
               SharedMemory -> return $ Integration SRust (SSharedMemory options) $ Just $ RustPasses.passes options liftCollectType
               M3 -> return $ Integration SRust (SM3 options) Nothing
    ".py" -> case arch of
               MultiProcessing-> return $ Integration SPython (SMultiProc options) $ Just $ PyPasses.passes options id
    _ -> throwError $ "No language integration defined for files with extension '" <> ext <> "'"
  apply integration comp

langInfo :: Text -> IO ()
-- ToDo: See import FIXME
langInfo lang | (T.toLower lang) == "rust" || (T.toLower lang) == "condrust" = putStrLn "No further information for the Rust integration available" -- = macro_support
langInfo lang | (T.toLower lang) == "python" = putStrLn "No further information for the Python integration available"
langInfo lang = putStrLn $ "Language not supported: " <> show lang
