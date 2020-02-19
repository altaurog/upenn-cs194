import Test.Tasty

import qualified TestHomework01 as TH01
import qualified TestHomework02 as TH02
import qualified TestHomework03 as TH03

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ TH01.tests
    , TH02.tests
    , TH03.tests
    ]
