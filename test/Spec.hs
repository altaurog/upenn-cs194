import Test.Tasty

import qualified TestHomework01 as TH01
import qualified TestHomework02 as TH02
import qualified TestHomework03 as TH03
import qualified TestHomework04 as TH04
import qualified TestHomework05 as TH05
import qualified TestHomework06 as TH06

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ TH01.tests
    , TH02.tests
    , TH03.tests
    , TH04.tests
    , TH05.tests
    , TH06.tests
    ]
