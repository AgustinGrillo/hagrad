module GradientTest
  ( gradientSpec,
  )
where

import Lib
import Test.Hspec

gradientSpec :: Spec
gradientSpec = describe "Gradient Computation Test" $ do
  let x1 = param 3 "p1"
  let x2 = param 10 "p2"
  let simpleUnaryLoss = cos x1
  let compoundUnaryLoss = exp (tan x1)
  let simpleBinaryLoss = 2 * x1
  let singleVariableSimpleBinaryLoss = (x1 ** 2) + x1
  let multiVariableSimpleBinaryLoss = x1 / x2
  let compoundLoss = tanh (x1 / x2) * sin x2 - sin x1

  describe "Feedforward evaluation:" $ do
    -- Loss = f(params) = value
    it "Constant function evaluates correctly." $ do
      let x = 11.0
      x `shouldBe` (11.0 :: Float)
