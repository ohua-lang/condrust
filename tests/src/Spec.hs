import Ohua.Prelude

-- import qualified ALangVerify
-- import qualified DFLowering
-- import qualified TailRecSpec
-- FIXME
-- import           PassesSpec
import Test.Hspec
import qualified TestALangUtils
import qualified TestAesonConvert

main :: IO ()
main =
    hspec $
       -- FIXME
       -- passesSpec
        -- DFLowering.generalLowering
        -- DFLowering.ifSpec
        -- DFLowering.seqSpec
        -- DFLowering.smapSpec
    --ALangVerify.currying
     do
        TestALangUtils.spec
        TestAesonConvert.spec
        -- TailRecSpec.passesSpec
