{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.Integration where

import Ohua.Prelude

import qualified Ohua.Frontend.Types as F (Integration, Lang, frontend)
import qualified Ohua.Backend.Types as B (Integration, lower)
import Ohua.Integration.Rust.Types (Rust(..))
import Ohua.Integration.Rust.Frontend ()
import Ohua.Integration.Rust.Backend ()


type FileExtension = Text
type Description = Text

type FullIntegration lang integ = 
        ( F.Integration lang
        , integ ~ F.Lang lang
        , B.Integration integ
        )

data Integration = forall lang integ. FullIntegration lang integ => I lang

class Apply integration where
    apply :: CompM m 
        => integration 
        -> (forall lang integ.
            FullIntegration lang integ
            => lang 
            -> m a)
        -> m a

instance Apply Integration where
    apply (I l) comp = comp l 

definedIntegrations :: [(FileExtension, Description, Integration)]
definedIntegrations = [(".rs", "Rust integration", I $ Rust ())]

runIntegration :: CompM m 
                => Text 
                -> (forall lang integ.
                    FullIntegration lang integ
                    => lang -> m a)
                -> m a
runIntegration ext comp
    | Just a <- find ((== ext) . view _1) definedIntegrations = apply (a ^. _3) comp
        
    | otherwise =
        throwError $ "No language integration defined for files with extension '" <> ext <> "'"