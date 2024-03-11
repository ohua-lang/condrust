{-#LANGUAGE  UndecidableInstances #-}
module Ohua.Frontend.PPrint where

import Ohua.Commons.Prelude
-- import qualified Ohua.Commons.Types.Vector as V

import Ohua.Frontend.Lang

import Data.Text.Prettyprint.Doc as PP

import Data.Text.Prettyprint.Doc.Render.Text

import Data.Text as T (Text)
import qualified Data.Text.IO as LT
import qualified Data.List.NonEmpty as NE



prettyExpr :: Pretty a => a -> T.Text
prettyExpr = renderStrict . layoutSmart ohuaDefaultLayoutOpts . pretty

prettyExprM :: Pretty a => a -> IO ()
prettyExprM = LT.putStr . prettyExpr

afterLetIndent :: Int
afterLetIndent = 0

instance Pretty (Pat ty res) where
    pretty (VarP b ty) = pretty b <> "::" <> pretty ty
    pretty (TupP pats) = align $ tupled $ map pretty (NE.toList pats)

instance Pretty (Expr embExpr annot ty res []) where
    pretty (VarE bnd _ty) = pretty bnd
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

    pretty (AppE x xs) = pretty x <> align (tupled $ toList $  map pretty xs)
    pretty (LamE ps b) = sep ["λ" <+>
                    align
                        (sep [ sep (map pretty (toList ps) <> ["->"])
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
    pretty (WhileE e1 e2) = sep ["while" <+>
                        align 
                        (sep [ pretty e1 <> ":"
                             , pretty e2
                             ])]
    pretty (StateFunE s meth mCall) = pretty s <+> "."<+> pretty meth <+> pretty mCall
    pretty (StmtE e c) = vsep $ hsep [pretty e, ";"] : [pretty c]
    pretty (TupE (e:|es)) = hsep [align $ tupled $ map pretty (e:es)]

instance Pretty (Expr embExpr annot ty res NonEmpty) where
    pretty (VarE bnd _ty) = pretty bnd
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

    pretty (AppE x xs) = pretty x <> align (tupled $ toList $  map pretty xs)
    pretty (LamE ps b) = sep ["λ" <+>
                    align
                        (sep [ sep (map pretty (toList ps) <> ["->"])
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
    pretty (WhileE e1 e2) = sep ["while" <+>
                        align 
                        (sep [ pretty e1 <> ":"
                             , pretty e2
                             ])]
    pretty (StateFunE s meth mCall) = pretty s <+> "." <+> pretty meth <+>pretty mCall
    pretty (StmtE e c) = vsep $ hsep [pretty e, ";"] : [pretty c]
    pretty (TupE (e:|es)) = hsep [align $ tupled $ map pretty (e:es)]

instance Pretty (MethodRepr ty res) where
    pretty (MethodUnres bnd) = pretty bnd
    pretty (MethodRes qb ty ) = pretty qb 