{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F (Integration)
import qualified Ohua.Backend.Types as B (Integration)
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()


data Integration = forall lang. (F.Integration lang, B.Integration lang) => Integration lang
type FileExtension = Text
type Description = Text

definedIntegrations :: [(FileExtension, Description, Integration)]
definedIntegrations = [(".rs", "Rust integration", Integration Rust)]

getIntegration :: Text -> Integration
getIntegration ext
    | Just a <- find ((== ext) . view _1) definedIntegrations = a ^. _3
    | otherwise =
        error $ "No language integration defined for files with extension '" <> ext <> "'"