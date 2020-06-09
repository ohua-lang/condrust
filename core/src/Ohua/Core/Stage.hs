module Ohua.Core.Stage where

import Ohua.Core.Prelude

import qualified Data.Text.Lazy.IO as LT
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Lazy as T (concat, Text)


stage ::
       (MonadReadEnvironment m, MonadIO m, Pretty code)
    => StageName
    -> code
    -> m ()
stage stName code = do
    stageH <- fromEnv $ options . stageHandling
    let (dumpInstructions, shouldAbort) = stageH stName
    case dumpInstructions of
        DumpPretty ->
            liftIO $
            LT.writeFile (toString $ stName <> ".dump") $ gen
        DumpStdOut ->
            liftIO $
            LT.putStr $ boundary <> stageHeader <> gen <> boundary
        Don'tDump -> pure ()
    when shouldAbort exitSuccess
    where
        gen = renderLazy $ layoutSmart ohuaDefaultLayoutOpts $ pretty code
        boundary = "\n" <>(T.concat $ take 20 $ repeat ("-"::T.Text)) <> "\n"
        stageHeader = "stage: " <> fromStrict stName <> "\n\n"

resolvedAlang :: StageName
resolvedAlang = "alang-resolved"

ssaAlang :: StageName
ssaAlang = "alang-ssa"

normalizedAlang :: StageName
normalizedAlang = "alang-normalized"

customAlangPasses :: StageName
customAlangPasses = "alang-custom"

coreAlang :: StageName
coreAlang = "alang-core"

initialDflang :: StageName
initialDflang = "dflang-initial"

customDflang :: StageName
customDflang = "dflang-custom"

coreDflang :: StageName
coreDflang = "dflang-core"

literalsALang :: StageName
literalsALang = "alang-literals"

unitFunctionsALang :: StageName
unitFunctionsALang = "alang-unit-functions"

smapTransformationALang :: StageName
smapTransformationALang = "alang-smap-transformation"

conditionalsTransformationALang :: StageName
conditionalsTransformationALang = "alang-condtionals-transformation"

seqTransformationALang :: StageName
seqTransformationALang = "alang-seq-transformation"


knownStages :: [StageName]
knownStages =
    [ resolvedAlang
    , ssaAlang
    , normalizedAlang
    , customAlangPasses
    , coreAlang
    , initialDflang
    , customDflang
    , coreDflang
    , literalsALang
    , unitFunctionsALang
    , smapTransformationALang
    , conditionalsTransformationALang
    , seqTransformationALang
    ]
