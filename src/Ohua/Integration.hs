{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F 
import qualified Ohua.Backend.Types as B
import Ohua.Integration.Lang
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Architecture.SharedMemory ()
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
        , F.Types (Language lang) ~ B.Types (Language lang)
        , B.Architecture (Architectures arch)
        , B.Lang (Architectures arch) ~ (Language lang)
        )

data Integration = 
    forall (lang::Lang) (arch::Arch). FullIntegration lang arch => I (Language lang) (Architectures arch)

class Apply integration where
    apply :: CompM m 
        => integration 
        -> (forall lang arch.
            FullIntegration lang arch
            => Language lang 
            -> Architectures arch
            -> m a)
        -> m a

instance Apply Integration where
    apply (I lang arch) comp = comp lang arch

definedIntegrations :: [(FileExtension, ArchId, Description, Integration)]
definedIntegrations = 
    [ (".rs", "sm", "Rust integration", I SRust SSharedMemory)
    , (".rs", "m3", "Rust integration", I SRust SM3)]

runIntegration :: CompM m 
                => Text 
                -> Text
                -> (forall lang arch.
                    FullIntegration lang arch
                    => Language lang -> Architectures arch -> m a)
                -> m a
runIntegration ext arch comp
    | Just a <- find (\int -> (int ^. _1 == ext) && (int ^. _2 == arch) ) definedIntegrations
        = apply (a ^. _4) comp
    | otherwise =
        throwError $ "No language integration defined for files with extension '" <> ext <> "'"