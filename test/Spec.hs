-- file Spec.hs

import LossTest
import GradientTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    lossSpec
    gradientSpec
