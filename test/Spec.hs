import Test.Tasty

import qualified TestHomework01 as TH01

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ TH01.tests ]
