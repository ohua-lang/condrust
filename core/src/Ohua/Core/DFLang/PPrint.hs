{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.Core.DFLang.PPrint where

import Ohua.Core.Prelude

import Data.Text.Prettyprint.Doc as PP

import Ohua.Core.ALang.PPrint ()
import Ohua.Core.DFLang.Lang


instance Pretty NormalizedExpr where
    pretty = \case
        (LetPureFun app cont) -> printIt app cont
        (LetStateFun app cont) -> printIt app cont
        (VarFun bnd) -> vsep [pretty bnd]
        where
            printIt app cont = vsep $ pretty app : [pretty cont]

instance Pretty (ABinding a) where
    pretty = pretty . unwrapABnd

instance Pretty (App a) where
    pretty (PureFun output fun inps) =
        hsep $
            [ "let"
            , align $ pretty output
            , "="
            , pretty fun
            ] <>
            [align $ tupled $ toList $ map pretty inps, "in"]
    pretty (StateFun (stateOut, out) fun stateIn inps) =
        hsep $
            [ "let"
            , align $ tupled [maybe "_" pretty stateOut, pretty out]
            , "="
            , pretty fun
            ] <>
            (pure . brackets . pretty) stateIn <>
            [align $ tupled $ toList $ map pretty inps, "in"]

instance Pretty DFVar where
    pretty = \case
        DFEnvVar he -> pretty he
        DFVar b -> pretty b
