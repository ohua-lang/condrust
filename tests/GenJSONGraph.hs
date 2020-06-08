import Ohua.Core.Prelude

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Ohua.Core.DFGraph
import           Ohua.Core.Serialize.JSON  ()
import           Ohua.Core.Types.Arbitrary ()
import           Test.QuickCheck


main :: IO ()
main = do
    l <- generate (arbitrary :: Gen OutGraph)

    print l

    L.writeFile "a-graph.json" $ encode l
