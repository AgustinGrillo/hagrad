-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib


main :: IO ()
main = hspec $ do
  describe "Test 1" $ do
    it "2 and 5 sums to 7" $ do
      add 2 5 `shouldBe` (7 :: Int)

