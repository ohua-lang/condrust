module Ohua.Integration.Python.Datapar where


import Ohua.Prelude hiding (concat)
import Ohua.Integration.Python.NewBackend
import Ohua.Integration.Python.Backend.Subset
import Ohua.Integration.Transform.DataPar (concat, joinFuture, spawnFuture, takeN)

procCount = 1

spawnWork:: Suite -> Suite 
spawnWork suite = suite