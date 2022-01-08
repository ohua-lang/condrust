module TestOptions where
import Ohua.Prelude


import Ohua.Core.Types.Environment (stageHandling, Options, transformRecursiveFunctions)
import Ohua.Core.Types.Stage (DumpCode(..), StageHandling)
import Ohua.Core.Stage

import Ohua.Compile.Config (intoStageHandling, Stage(..))


data DebugOptions = DebugOptions
  { printIRs :: Bool,
    showCodeDiff :: Bool
  }

data CompilationType
  = OhuaOnly
  | BuildTarget
  | RunTarget


instance Default DebugOptions where
  def = DebugOptions False False


withDebug :: Options -> Options
withDebug d = d & stageHandling .~ debugStageHandling

withRec :: Options -> Options
withRec d = d & transformRecursiveFunctions .~ True

debugStageHandling :: StageHandling
debugStageHandling =
    intoStageHandling DumpStdOut
        $ Just
            [ Stage resolvedAlang True False
            , Stage normalizedAlang True False
            , Stage coreDflang True False
            , Stage coreAlang True False
            , Stage initialDflang True False
            , Stage preControlSTCLangALang True False
            , Stage smapTransformationALang True False
            , Stage conditionalsTransformationALang True False
            , Stage seqTransformationALang True False
            , Stage postControlSTCLangALang True False
            , Stage normalizeAfterCorePasses True False
            , Stage customDflang True False
            , Stage finalDflang True False
            ]
