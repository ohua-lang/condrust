{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.Core.DFLang.PPrint where

import Ohua.Core.Prelude

import Data.Text.Prettyprint.Doc as PP

import Ohua.Core.ALang.PPrint ()
import Ohua.Core.DFLang.Lang


instance Pretty NormalizedDFExpr where
    pretty = \case
        (Let app cont) -> vsep $ pretty app : [pretty cont]
        (Var bnd) -> vsep [pretty bnd]

instance Pretty NormalizedExpr where
    pretty = \case
        (Let app cont) -> printIt app cont
        (Var bnd) -> vsep [pretty bnd]
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

instance Pretty (DFApp a) where
    pretty (PureDFFun output fun inps) =
        hsep $
            [ "let"
            , align $ pretty output
            , "="
            , pretty fun
            ] <>
            [align $ tupled $ toList $ map pretty inps, "in"]
    pretty (StateDFFun (stateOut, out) fun stateIn inps) =
        hsep $
            [ "let"
            , align $ tupled [maybe "_" pretty stateOut, pretty out]
            , "="
            , pretty fun
            ] <>
            (pure . brackets . pretty) stateIn <>
            [align $ tupled $ toList $ map pretty inps, "in"]

instance Pretty (OutData a) where
    pretty (Direct b) = pretty b
    pretty (Destruct ds) = align $ tupled $ map pretty $ toList ds
    pretty (Dispatch ds) = align $ tupled $ map pretty $ toList ds

instance Pretty DFVar where
    pretty = \case
        DFEnvVar he -> pretty he
        DFVar b -> pretty b
