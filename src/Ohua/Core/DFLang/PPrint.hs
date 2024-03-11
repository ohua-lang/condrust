{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.Core.DFLang.PPrint where

import Ohua.Core.Prelude
import qualified Ohua.Commons.Types.Vector as V

import Data.Text.Prettyprint.Doc as PP

import Ohua.Commons.Types.Reference ()
import Ohua.Core.ALang.PPrint ()
import Ohua.Core.DFLang.Lang

import Data.Text.Prettyprint.Doc.Render.Text

import Data.Text.Lazy as T (Text)
import qualified Data.Text.Lazy.IO as LT


prettyExpr :: Pretty a => a -> T.Text
prettyExpr = renderLazy . layoutSmart ohuaDefaultLayoutOpts . pretty

prettyExprM :: Pretty a => a -> IO ()
prettyExprM = LT.putStr . prettyExpr

instance Pretty (NormalizedDFExpr embExpr annot ty) where
    pretty = \case
        (Let app cont) -> vsep $ hsep ["let", pretty app, "in"] : [pretty cont]
        (Var atBnd ) -> vsep [pretty atBnd]

instance Pretty (NormalizedExpr embExpr annot ty) where
    pretty = \case
        (Let app cont) -> vsep $ hsep [pretty app, "in"] : [pretty cont]
        (Var atBnd) -> vsep [pretty atBnd]

instance Pretty (ATypedBinding a ty) where
    pretty atb = 
        let tb = unwrapTB atb
        in hsep [pretty $ asBnd tb, "::", pretty $ asType tb] 

instance Pretty (App fTy embExpr annot ty) where
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

instance Pretty (DFApp fTy embExpr annot ty) where
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
    pretty (CollectFun out dIn1 dIn2) =
        hsep $
            [ align $ pretty out
            , "="
            , "collectFun"
            ] <>
            [align $ tupled [pretty dIn1, pretty dIn2]]
    pretty (CtrlFun out cIn dIn) =
        hsep $
            [ align $ pretty out
            , "="
            , "ctrlFun"
            ] <>
            [align $ tupled [pretty cIn, pretty dIn]]
    pretty (SelectFun out cIn dIn1 dIn2) =
        hsep $
            [ align $ pretty out
            , "="
            , "selectFun"
            ] <>
            [align $ tupled [pretty cIn, pretty dIn1, pretty dIn2]]
   
            
instance Pretty (OutData bTy ty) where
    pretty (Direct b) = pretty b
    pretty (Destruct ds) = align $ tupled $ map pretty $ toList ds
    pretty (Dispatch ds) = align $ tupled $ map pretty $ toList ds

instance Pretty (DFVar bTy embExpr annot ty) where
    pretty = \case
        DFEnvVar _ he -> pretty he
        DFVar atb -> pretty atb
