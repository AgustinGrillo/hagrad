module LossTest
  ( lossSpec,
  )
where

import Lib
import Loss
import Operators
import Test.Hspec

lossSpec :: Spec
lossSpec = describe "Loss Test" $ do
  describe "Loss function evaluation:" $ do
    -- Loss = f(params) = value
    it "Constant function evaluates correctly." $ do
      let constant = 11.0
      let loss = createCoefLoss constant
      evaluate loss `shouldBe` (11.0 :: Float)

    it "Linear loss function evaluates correctly when input equals 0." $ do
      let param = Param 0.0 "p"
      let loss = createVarLoss param
      evaluate loss `shouldBe` (0.0 :: Float)

    it "Linear loss function evaluates correctly when input equals 1." $ do
      let param = Param 1.0 "p"
      let loss = createVarLoss param
      evaluate loss `shouldBe` (1.0 :: Float)

    it "Binary operator evaluates correctly." $ do
      let param1 = createVarLoss (Param 2.0 "p1")
      let param2 = createVarLoss (Param 8.0 "p2")
      let loss = createBinaryLoss multOperator param1 param2
      evaluate loss `shouldBe` (16.0 :: Float)

    it "Unary operator evaluates correctly." $ do
      let param = createVarLoss (Param pi "p")
      let loss = createUnaryLoss cosOperator param
      evaluate loss `shouldBe` (-1.0 :: Float)

    it "Compound function evaluates correctly." $ do
      let x1 = createVarLoss (Param 3 "p1")
      let x2 = createVarLoss (Param 2 "p2")
      let k3 = createCoefLoss 24
      let f1 = createUnaryLoss cosOperator x1
      let f2 = createBinaryLoss powerOperator f1 (createCoefLoss 3.0)
      let f3 = createBinaryLoss plusOperator f2 k3
      let f4 = createUnaryLoss expOperator x2
      let loss = createBinaryLoss multOperator f3 f4
      evaluate loss `shouldBe` (170.16791 :: Float)

-- describe "Loss function feedforward:" $ do
--   -- Loss = f(params) = value
--   it "Feedforward over constant function returns correct value." $ do
--     let constant = 11.0
--     let loss = Coef constant
--     feedforward loss `shouldBe` (11.0 :: Float)

-- describe "One-dimensional gradient:" $ do
--   it "derivate of f=x with respect to x equals 1" $ do
--     -- L = x
--     -- dL/dx (x=0) = 1.0
--     let x = Param 0.0
--     let loss = Loss x
--     ___ = {function, , }
--     grad loss `shouldBe` (1 :: Float)

-- it "derivate of f=2*x with respect to x equals 2" $ do
