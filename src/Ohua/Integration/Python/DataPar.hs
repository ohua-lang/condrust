module Ohua.Integration.Python.DataPar where


import Ohua.Prelude hiding (concat)
import Ohua.Integration.Python.Backend
import Ohua.Integration.Python.Backend.Subset
import Ohua.Integration.Transform.DataPar (concat, joinFuture, spawnFuture, takeN)

procCount = 1

spawnWork:: Suite -> Suite 
spawnWork suite = suite