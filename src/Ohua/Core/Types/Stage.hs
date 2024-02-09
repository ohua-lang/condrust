module Ohua.Core.Types.Stage where

import Universum

type StageName = Text
type AbortCompilation = Bool
data DumpCode
    = Don'tDump
    | DumpPretty
    | DumpStdOut
type StageHandling = StageName -> (DumpCode, AbortCompilation)
