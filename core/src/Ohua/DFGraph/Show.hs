-- This is necessary as otherwise, for some reason the type inference
-- finds the lists to be ambiguous. I don't know why, it seems to me
-- as though it should be obvious, since the argument to `vsep` and
-- `vcat` and the likes aren't polymorphic.
{-# LANGUAGE NoOverloadedLists #-}

module Ohua.DFGraph.Show where

import Ohua.Prelude

import qualified Data.Text as T
import Text.PrettyPrint.Boxes hiding ((<>))

import Ohua.DFGraph

-- TODO show compound arcs
-- | TODO show return arc
asTable :: OutGraph -> T.Text
asTable (OutGraph ops (Arcs direct compound state) _) =
    T.pack $
    render $
    vsep
        1
        left
        [ text "Operators"
        , hsep 4 top [idList, typeList]
        , text "Direct Arcs"
        , hsep
              4
              top
              [sourceList direct sourceToBox, targetList direct targetToBox]
        , text "Compound Arcs"
        , hsep
              4
              top
              [ sourceList compound $ hsep 1 left . map targetToBox
              , targetList compound targetToBox
              ]
        , text "State Arcs"
        , hsep
              4
              top
              [ sourceList state sourceToBox
              , targetList state $ text . show . unwrap
              ]
        ]
  where
    idList =
        vcat right $ text "ids" : map (text . show . unwrap . operatorId) ops
    typeList = vcat left $ text "types" : map (text . show . operatorType) ops
    sourceList arcs' f = vcat left $ (text "source" :) $ f . source <$> arcs'
    sourceToBox =
        \case
            EnvSource i ->
                case i of
                    UnitLit -> "()"
                    NumericLit n -> text $ show n
                    EnvRefLit n -> text $ "$" <> show (unwrap n)
                    FunRefLit _fr -> error "Not supported"
            LocalSource t -> targetToBox t
    targetList arcs' f =
        vcat left $ (text "target" :) $ (f . target) `map` arcs'
    targetToBox (Target op idx) = text $ show (unwrap op) ++ " @ " ++ show idx
