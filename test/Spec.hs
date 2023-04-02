-- file Spec.hs

import LossTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    lossSpec
