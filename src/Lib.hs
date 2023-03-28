module Lib (module Lib) where

tester :: IO ()
tester = putStrLn $ "test"

--- Custom Data Types
type UnOp = (Float -> Float)

type BinOp = (Float -> Float -> Float)

data Param = Param Float

data Loss
  = Unary {unOperator :: UnOp, arg :: Loss, value :: Float, grad :: Float}
  | Binary {binOperator :: BinOp, leftArg :: Loss, rightArg :: Loss, value :: Float, grad :: Float}
  | Var {varArg :: Param, value :: Float, grad :: Float}
  | Coef {coefArg :: Float, value :: Float, grad :: Float}

createUnaryLoss :: UnOp -> Loss -> Loss
createUnaryLoss op arg =
  Unary
    { unOperator = op,
      arg = arg,
      value = 0.0,
      grad = 0.0
    }

createBinaryLoss :: BinOp -> Loss -> Loss -> Loss
createBinaryLoss op leftArg rightArg =
  Binary
    { binOperator = op,
      leftArg = leftArg,
      rightArg = rightArg,
      value = 0.0,
      grad = 0.0
    }

createVarLoss :: Param -> Loss
createVarLoss variable =
  Var
    { varArg = variable,
      value = 0.0,
      grad = 0.0
    }

createCoefLoss :: Float -> Loss
createCoefLoss constant =
  Coef
    { coefArg = constant,
      value = 0.0,
      grad = 0.0
    }

evaluate :: Loss -> Float
evaluate (Coef constant _ _) = constant
evaluate (Var (Param param) _ _) = param
evaluate (Binary f x1 x2 _ _) = f (evaluate x1) (evaluate x2)
evaluate (Unary f x _ _) = f (evaluate x)
