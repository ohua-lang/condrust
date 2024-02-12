module Ohua.Core.Feature.TailRec where


import Ohua.Core.Prelude

import Ohua.Core.Compile.Configuration

import Ohua.Core.Feature.TailRec.Passes.ALang
import Ohua.Core.Feature.TailRec.Passes.DFLang


loadTailRecPasses :: (Show embExpr) => Bool -> CustomPasses embExpr ty -> CustomPasses embExpr ty
loadTailRecPasses enabled passes =
    passes
        { passBeforeNormalize =
              findTailRecs enabled >=> passBeforeNormalize passes
        , passAfterNormalize =
              -- FIXME this removes the hofs for tail recursion already.
              --       there should be a separate stage for this!
              --       for now, we run these passes only after all other passes ran.
              passAfterNormalize passes >=>
              (if enabled
                   then rewriteAll
                   else pure)
        , passAfterDFLowering =
              passAfterDFLowering passes <=<
              if enabled
                  then recurLowering -- . collapseNth (== recurStartMarker)
                  else pure
        }
