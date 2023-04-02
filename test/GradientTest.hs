module GradientTest
  ( gradientSpec,
  )
where

import Lib
import Loss
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
    it "over simple unary loss." $ do
      let ff = feedforward simpleUnaryLoss
      value ff `shouldBe` -0.9899925
    it "over compound unary loss." $ do
      let ff = feedforward compoundUnaryLoss
      value ff `shouldBe` 0.8671472
    it "over simple binary loss." $ do
      let ff = feedforward simpleBinaryLoss
      value ff `shouldBe` 6.0
    it "over single repeated variable simple binary loss." $ do
      let ff = feedforward singleVariableSimpleBinaryLoss
      value ff `shouldBe` 12
    it "over multi variable simple binary loss." $ do
      let ff = feedforward multiVariableSimpleBinaryLoss
      value ff `shouldBe` 0.3
    it "over compound loss." $ do
      let ff = feedforward compoundLoss
      value ff `shouldBe` -1.736021e-2

  describe "Gradient evaluation:" $ do
    it "over simple unary loss." $ do
      let loss = (backpropagation . feedforward) simpleUnaryLoss
      let grad = gradient loss x1
      grad `shouldBe` -0.14112 
    it "over compound unary loss." $ do
      let loss = (backpropagation . feedforward) compoundUnaryLoss
      let grad = gradient loss x1
      grad `shouldBe` 0.8847673 
    it "over simple binary loss." $ do
      let loss = (backpropagation . feedforward) simpleBinaryLoss
      let grad = gradient loss x1
      grad `shouldBe` 2.0
    it "over single repeated variable simple binary loss." $ do
      let loss = (backpropagation . feedforward) singleVariableSimpleBinaryLoss
      let grad = gradient loss x1
      grad `shouldBe` 7
    it "over multi variable simple binary loss." $ do
      let loss = (backpropagation . feedforward) multiVariableSimpleBinaryLoss
      let grad_x1 = gradient loss x1
      let grad_x2 = gradient loss x2
      grad_x1 `shouldBe` 0.1
      grad_x2 `shouldBe` -0.03
    it "over compound loss." $ do
      let loss = (backpropagation . feedforward) compoundLoss
      let grad_x1 = gradient loss x1
      let grad_x2 = gradient loss x2
      grad_x1 `shouldBe` -1.0397779
      grad_x2 `shouldBe` -0.2294965 
