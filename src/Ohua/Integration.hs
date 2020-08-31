{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F (Integration, Lang, frontend)
import qualified Ohua.Backend.Types as B (Integration, lower, Architecture, Integ)
import Ohua.Integration.Rust.Types (Rust(..))
import Ohua.Integration.Rust.Architecture (Architectures(..))
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()


type FileExtension = Text
type ArchId = Text
type Description = Text

type FullIntegration lang integ arch = 
        ( F.Integration lang
        , integ ~ F.Lang lang
        , B.Integration integ
        , B.Architecture arch
        , integ ~ B.Integ arch
        )

data Integration = 
    forall lang integ arch. FullIntegration lang integ arch => I lang arch

class Apply integration where
    apply :: CompM m 
        => integration 
        -> (forall lang integ arch.
            FullIntegration lang integ arch
            => lang 
            -> arch
            -> m a)
        -> m a

instance Apply Integration where
    apply (I lang arch) comp = comp lang arch

definedIntegrations :: [(FileExtension, ArchId, Description, Integration)]
definedIntegrations = [(".rs", "sm", "Rust integration", I Rust SharedMemory)]

runIntegration :: CompM m 
                => Text 
                -> Text
                -> (forall lang integ arch.
                    FullIntegration lang integ arch
                    => lang -> arch -> m a)
                -> m a
runIntegration ext _arch comp
    | Just a <- find ((== ext) . view _1) definedIntegrations = apply (a ^. _4) comp
    | otherwise =
        throwError $ "No language integration defined for files with extension '" <> ext <> "'"