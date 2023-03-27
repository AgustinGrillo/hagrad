-- file Spec.hs

import Test.Hspec
import Lib 

main :: IO ()
main = hspec $ do
  describe "Loss function evaluation:" $ do
      -- Loss = f(params) = value
    it "Constant function evaluates correctly." $ do
      let constant = 11.0
      let loss =  Coef constant :: Loss
      evaluate loss `shouldBe` (11.0 :: Float)

    it "Linear loss function evaluates correctly when input equals 0." $ do
      let param = Param 0.0
      let loss = Var param :: Loss
      evaluate loss `shouldBe` (0.0 :: Float)

    it "Linear loss function evaluates correctly when input equals 1." $ do
      let param = Param 1.0
      let loss = Var param :: Loss
      evaluate loss `shouldBe` (1.0 :: Float)

    it "Binary operator evaluates correctly." $ do
      let param1 = Param 2.0
      let param2 = Param 8.0
      let loss = Binary (*) (Var param1) (Var param2) :: Loss
      evaluate loss `shouldBe` (16.0 :: Float)

    it "Unary operator evaluates correctly." $ do
      let param = Param pi
      let loss = Unary (cos) (Var param) :: Loss
      evaluate loss `shouldBe` (-1.0 :: Float)

  -- describe "One-dimensional gradient:" $ do
  --   it "derivate of f=x with respect to x equals 1" $ do
  --     -- L = x
  --     -- dL/dx (x=0) = 1.0
  --     let x = Param 0.0
  --     let loss = Loss x
  --     ___ = {function, , }
  --     grad loss `shouldBe` (1 :: Float)

    -- it "derivate of f=2*x with respect to x equals 2" $ do
