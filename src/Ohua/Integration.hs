{-# LANGUAGE ConstraintKinds #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F (Integration, frontend)
import qualified Ohua.Backend.Types as B (Integration, backend)
import Ohua.Integration.Rust.Types (RustLang (Empty))
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()

type FileExtension = Text
type Description = Text

-- {-# LANGUAGE ExistentialQuantification #-}
-- data Integration = forall lang. (F.Integration lang, B.Integration lang) => Integration lang
-- without existential types
data Integration = Rust RustLang

instance F.Integration Integration where
    frontend f (Rust lang) = first Rust <$> F.frontend f lang 

instance B.Integration Integration where
    backend ns (Rust lang) = B.backend ns lang

definedIntegrations :: [(FileExtension, Description, Integration)]
definedIntegrations = [(".rs", "Rust integration", Rust Empty)]

getIntegration :: Text -> Integration
getIntegration ext
    | Just a <- find ((== ext) . view _1) definedIntegrations = a ^. _3
    | otherwise =
        error $ "No language integration defined for files with extension '" <> ext <> "'"