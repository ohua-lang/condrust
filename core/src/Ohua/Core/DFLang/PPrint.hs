{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.Core.DFLang.PPrint where

import Ohua.Core.Prelude
import qualified Ohua.Types.Vector as V

import Data.Text.Prettyprint.Doc as PP

import Ohua.Core.ALang.PPrint ()
import Ohua.Core.DFLang.Lang

import Data.Text.Prettyprint.Doc.Render.Text

import Data.Text.Lazy as T (Text)
import qualified Data.Text.Lazy.IO as LT


prettyExpr :: Pretty a => a -> T.Text
prettyExpr = renderLazy . layoutSmart ohuaDefaultLayoutOpts . pretty

prettyExprM :: Pretty a => a -> IO ()
prettyExprM = LT.putStr . prettyExpr

instance Pretty (NormalizedDFExpr ty) where
    pretty = \case
        (Let app cont) -> vsep $ hsep ["let", pretty app, "in"] : [pretty cont]
        (Var bnd _ ) -> vsep [pretty bnd]

instance Pretty (NormalizedExpr ty) where
    pretty = \case
        (Let app cont) -> vsep $ hsep [pretty app, "in"] : [pretty cont]
        (Var bnd _) -> vsep [pretty bnd]

instance Pretty (ABinding a) where
    pretty = pretty . unwrapABnd

instance Pretty (App a ty) where
    pretty (PureFun output fun inps) =
        hsep $
            [ align $ pretty output
            , "="
            , pretty fun
            ] <>
            [align $ tupled $ toList $ map pretty inps]
    pretty (StateFun (stateOut, out) fun stateIn inps) =
        hsep $
            [ align $ tupled [maybe "_" pretty stateOut, pretty out]
            , "="
            , pretty fun
            ] <>
            (pure . brackets . pretty) stateIn <>
            [align $ tupled $ toList $ map pretty inps]

instance Pretty (DFApp a ty) where
    pretty (PureDFFun output fun inps) =
        hsep $
            [ align $ pretty output
            , "="
            , pretty fun
            ] <>
            [align $ tupled $ toList $ map pretty inps]
    pretty (StateDFFun (stateOut, out) fun stateIn inps) =
        hsep $
            [ align $ tupled [maybe "_" pretty stateOut, pretty out]
            , "="
            , pretty fun
            ] <>
            (pure . brackets . pretty) stateIn <>
            [align $ tupled $ toList $ map pretty inps]
    pretty (RecurFun fOut rOut rOuts iIns rIns cIn fIn) =
        hsep $
            [ align $ tupled [pretty fOut, pretty rOut, pretty $ V.toList rOuts]
            , "="
            , "recurFun"
            ] <>
            [align $ tupled [pretty $ V.toList iIns, pretty $ V.toList rIns, pretty cIn, pretty fIn]]
    pretty (SMapFun (dOut, ctrlOut, sizeOut) dIn) =
        hsep $
            [ align $ tupled [pretty dOut, pretty ctrlOut, pretty sizeOut]
            , "="
            , "smapFun"
            ] <>
            [align $ tupled [pretty dIn]]
    pretty (IfFun (trueOut,falseOut) dIn) =
        hsep $
            [ align $ tupled [pretty trueOut, pretty falseOut]
            , "="
            , "ifFun"
            ] <>
            [align $ tupled [pretty dIn]]
            
instance Pretty (OutData a) where
    pretty (Direct b) = pretty b
    pretty (Destruct ds) = align $ tupled $ map pretty $ toList ds
    pretty (Dispatch ds) = align $ tupled $ map pretty $ toList ds

instance Pretty (DFVar a ty) where
    pretty = \case
        DFEnvVar _ he -> pretty he
        DFVar _ b -> pretty b
