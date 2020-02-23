import Test.Tasty

import qualified TestHomework01 as TH01
import qualified TestHomework02 as TH02
import qualified TestHomework03 as TH03
import qualified TestHomework04 as TH04

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ TH01.tests
    , TH02.tests
    , TH03.tests
    , TH04.tests
    ]
