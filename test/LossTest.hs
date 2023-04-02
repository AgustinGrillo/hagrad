module LossTest
  ( lossSpec,
  )
where

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

    it "Simplified sintax evaluates correctly." $ do
      let x1 = createVarLoss (Param 3 "p1")
      let x2 = createVarLoss (Param 2 "p2")
      let loss = (((cos x1) ** 3) + 24) * (exp x2)
      evaluate loss `shouldBe` (170.16791 :: Float)

