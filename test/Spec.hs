-- file Spec.hs

import Test.Hspec
import Lib 

main :: IO ()
main = hspec $ do
  describe "Loss function evaluation:" $ do
    it "Linear loss function evaluates correctly when input equals 0." $ do
      -- Loss = f(params) = value
      let param = Param 0.0
      let loss = Var param :: Loss
      evaluate loss `shouldBe` (0.0 :: Float)

    it "Linear loss function evaluates correctly when input equals 1." $ do
      -- Loss = f(params) = value
      let param = Param 1.0
      let loss = Var param :: Loss
      evaluate loss `shouldBe` (1.0 :: Float)

    it "Cuadratic loss functions evaluates correctly when input equals 2." $ do
      -- Loss = f(params) = value
      let param = Param 2.0
      let loss = Op "*" (Var param) (Var param) :: Loss
      evaluate loss `shouldBe` (4.0 :: Float)

  -- describe "One-dimensional gradient:" $ do
  --   it "derivate of f=x with respect to x equals 1" $ do
  --     -- L = x
  --     -- dL/dx (x=0) = 1.0
  --     let x = Param 0.0
  --     let loss = Loss x
  --     ___ = {function, , }
  --     grad loss `shouldBe` (1 :: Float)

    -- it "derivate of f=2*x with respect to x equals 2" $ do
