module Ohua.Frontend.PPrint where

import Ohua.Prelude
import qualified Ohua.Types.Vector as V

import Ohua.Frontend.Lang

import Data.Text.Prettyprint.Doc as PP

import Data.Text.Prettyprint.Doc.Render.Text

import Data.Text as T (Text)
import qualified Data.Text.IO as LT


prettyExpr :: Pretty a => a -> T.Text
prettyExpr = renderStrict . layoutSmart ohuaDefaultLayoutOpts . pretty

prettyExprM :: Pretty a => a -> IO ()
prettyExprM = LT.putStr . prettyExpr

afterLetIndent :: Int
afterLetIndent = 0

instance Pretty (Pat ty) where
    pretty (VarP b ty) = pretty b
    pretty (TupP pats) = align $ tupled $ map pretty pats
    pretty UnitP = "()"

instance Pretty (Expr ty) where
    pretty (VarE bnd ty) = pretty bnd
    pretty (LitE l) = pretty l
    pretty (LetE p app cont) =
        sep
        [ "let" <+>
            align
                (hang afterLetIndent $
                sep
                    [ hsep [pretty p] <+>
                        "="
                    , pretty app
                    ]) <+>
            "in"
        , align (pretty cont)
        ]

    pretty (AppE x xs) = pretty x <> align (tupled $ map pretty xs)
    pretty (LamE ps b) = sep ["λ" <+>
                    align
                        (sep [ sep (map pretty ps <> ["->"])
                             , pretty b
                             ])]
    pretty (IfE c t f) = sep ["if" <+>
                    align
                        (sep [ sep [pretty c]
                             , pretty t
                             , pretty f
                             ])]
    pretty (MapE b d) = sep ["map" <+>
                    align
                        (sep [ pretty b
                             , pretty d
                             ])]
    pretty (BindE s t) = hsep [brackets $ pretty s, pretty t]
    pretty (StmtE e c) = vsep $ hsep [pretty e, ";"] : [pretty c]
    pretty (SeqE e c) = vsep $ hsep ["seq"] : [pretty e, pretty c]
    -- TODO print tuple type
    pretty (TupE _ty es) = hsep [align $ tupled $ map pretty es]
