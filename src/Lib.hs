module Lib (module Lib) where

tester :: IO ()
tester = putStrLn $ "test"

--- Custom Data Types

data UnOp = UnOp {uName :: String, uOperator :: (Float -> Float), uDerivative :: (Float -> Float)}

data BinOp = BinOp {bName :: String, bOperator :: (Float -> Float -> Float), bDerivative :: (Float -> Float)}

data Param = Param Float

data Loss
  = Unary {unOperator :: UnOp, arg :: Loss, value :: Float, grad :: Float}
  | Binary {binOperator :: BinOp, leftArg :: Loss, rightArg :: Loss, value :: Float, grad :: Float}
  | Var {varArg :: Param, value :: Float, grad :: Float}
  | Coef {coefArg :: Float, value :: Float, grad :: Float}

instance Show Loss where
  show loss = customShow loss 0
    where
      customShow :: Loss -> Int -> String
      customShow (Coef constant v g) l = treePrinter l ++ " ( " ++ show constant ++ " ) " ++ " -  V: " ++ show v ++ "  G: " ++ show g
      customShow (Var (Param param) v g) l = treePrinter l ++ " ( " ++ show param ++ " ) " ++ " -  V: " ++ show v ++ "  G: " ++ show g
      customShow (Binary f x1 x2 v g) l = treePrinter l ++ " ( " ++ bName f ++ " ) " ++ " -  V: " ++ show v ++ "  G: " ++ show g ++ customShow x1 (l + 1) ++ customShow x2 (l + 1)
      customShow (Unary f x v g) l = treePrinter l ++ " ( " ++ uName f ++ " ) " ++ " -  V: " ++ show v ++ "  G: " ++ show g ++ customShow x (l + 1)

      treePrinter :: Int -> String
      treePrinter indentation = "\n" ++ tabs ++ "│\n" ++ tabs ++ "└─"
        where
          tabs = (replicate (2 * indentation) ' ')

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
evaluate (Binary f x1 x2 _ _) = (bOperator f) (evaluate x1) (evaluate x2)
evaluate (Unary f x _ _) = (uOperator f) (evaluate x)

--- Supported operators

cosOperator = UnOp "cos" cos id

sinOperator = UnOp "sin" sin id

tanOperator = UnOp "tan" tan id

expOperator = UnOp "exp" exp id

tanhOperator = UnOp "tanh" tanh id

plusOperator = BinOp "+" (+) id

minusOperator = BinOp "-" (-) id

divOperator = BinOp "/" (/) id

multOperator = BinOp "*" (*) id

powerOperator = BinOp "^" (**) id

--- Test

testC = createCoefLoss 10

testP = createVarLoss (Param 1)

testUO = createUnaryLoss cosOperator testC

testBO = createBinaryLoss plusOperator testUO testP

-- FeedForward

feedforward :: Loss -> Loss
feedforward loss@(Coef constant _ _) = Coef constant (evaluate loss) 0.0
feedforward loss@(Var param _ _) = Var param (evaluate loss) 0.0
feedforward loss@(Binary f x1 x2 _ _) = Binary f (feedforward x1) (feedforward x2) (evaluate loss) 0.0
feedforward loss@(Unary f x _ _) = Unary f (feedforward x) (evaluate loss) 0.0
