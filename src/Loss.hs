module Loss (module Loss) where

import Operators

--- Custom Data Types

-- TODO: Make constructors private
data Param = Param {pValue :: Float, pName :: String} deriving (Eq)

data Loss
  = Unary {unOperator :: UnOp, arg :: Loss, value :: Float, grad :: Float}
  | Binary {binOperator :: BinOp, leftArg :: Loss, rightArg :: Loss, value :: Float, grad :: Float}
  | Var {varArg :: Param, value :: Float, grad :: Float}
  | Coef {coefArg :: Float, value :: Float, grad :: Float}

instance Num Loss where
  leftLoss + rightLoss = createBinaryLoss plusOperator leftLoss rightLoss
  leftLoss * rightLoss = createBinaryLoss multOperator leftLoss rightLoss
  leftLoss - rightLoss = createBinaryLoss plusOperator leftLoss rightLoss
  fromInteger i = createCoefLoss (fromInteger i)
  abs loss = error "Not implemented"
  signum loss = error "Not implemented"

instance Fractional Loss where
  leftLoss / rightLoss = createBinaryLoss divOperator leftLoss rightLoss
  fromRational i = createCoefLoss (fromRational i)

instance Floating Loss where
  leftLoss ** rightLoss = createBinaryLoss powerOperator leftLoss rightLoss
  pi = createCoefLoss pi
  exp loss = createUnaryLoss expOperator loss
  sin loss = createUnaryLoss sinOperator loss
  cos loss = createUnaryLoss cosOperator loss
  tan loss = createUnaryLoss tanOperator loss
  tanh loss = createUnaryLoss tanhOperator loss
  log loss = error "Not implemented"
  asin loss = error "Not implemented"
  acos loss = error "Not implemented"
  atan loss = error "Not implemented"
  sinh loss = error "Not implemented"
  cosh loss = error "Not implemented"
  asinh loss = error "Not implemented"
  acosh loss = error "Not implemented"
  atanh loss = error "Not implemented"

instance Show Loss where
  show loss = customShow loss 0
    where
      customShow :: Loss -> Int -> String
      customShow (Coef constant v g) l = treePrinter l ++ " ( " ++ show constant ++ " ) " ++ " -  V: " ++ show v ++ "  G: " ++ show g
      customShow (Var (Param param _) v g) l = treePrinter l ++ " ( " ++ show param ++ " ) " ++ " -  V: " ++ show v ++ "  G: " ++ show g
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
evaluate (Var (Param param _) _ _) = param
evaluate (Binary f x1 x2 _ _) = (bOperator f) (evaluate x1) (evaluate x2)
evaluate (Unary f x _ _) = (uOperator f) (evaluate x)
