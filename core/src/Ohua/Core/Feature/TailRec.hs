module Ohua.Core.Feature.TailRec where


import Ohua.Core.Prelude

import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.Passes (collapseNth)

import Ohua.Core.Feature.TailRec.Passes.ALang
import Ohua.Core.Feature.TailRec.Passes.DFLang


loadTailRecPasses :: Bool -> CustomPasses -> CustomPasses
loadTailRecPasses enabled passes =
    passes
        { passBeforeNormalize =
              findTailRecs enabled >=> passBeforeNormalize passes
        , passAfterNormalize =
              (if enabled
                   then rewriteAll
                   else pure) >=>
              passAfterNormalize passes
        , passAfterDFLowering =
              passAfterDFLowering passes .
              if enabled
                  then recurLowering . collapseNth (== recurStartMarker)
                  else id
        }
