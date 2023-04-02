module Lib (module Lib) where

tester :: IO ()
tester = putStrLn $ "test"

--- Custom Data Types

data UnOp = UnOp {uName :: String, uOperator :: (Float -> Float), uDerivative :: (Float -> Float)}

data BinOp = BinOp {bName :: String, bOperator :: (Float -> Float -> Float), leftDerivative :: (Float -> Float -> (Float -> Float)), rightDerivative :: (Float -> Float -> (Float -> Float))}

data Param = Param {pValue :: Float, pName :: String} deriving (Eq)

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

--- Supported operators

cosOperator = UnOp "cos" cos ((* (-1)) . sin)

sinOperator = UnOp "sin" sin cos

tanOperator = UnOp "tan" tan ((1 /) . (** 2) . cos)

expOperator = UnOp "exp" exp exp

tanhOperator = UnOp "tanh" tanh ((1 /) . (** 2) . cosh)

plusOperator = BinOp "+" (+) (\f g -> const 1) (\f g -> const 1) -- d(f+g)/df = 1   d(f+g)/dg = 1

minusOperator = BinOp "-" (-) (\f g -> const 1) (\f g -> const (-1)) -- d(f-g)/df = 1   d(f-g)/dg = -1

divOperator = BinOp "/" (/) (\f g -> const (1 / g)) (\f g -> const (-f / (g ** 2))) -- d(f/g)/df = 1/g   d(f/g)/dg = -f/g^2

multOperator = BinOp "*" (*) (\f g -> const g) (\f g -> const f) -- d(f.g)/df = g   d(f.g)/dg = f

powerOperator = BinOp "^" (**) (\f g -> const (g * f ** (g - 1))) (\f g -> const ((log f) * f ** (g))) -- d(f^g)/df = g.f^(g-1)   d(f^g)/dg = (f^g).ln(f)

--- Test

testC = createCoefLoss 10

testP1 = createVarLoss (Param 10 "p1")

testP2 = createVarLoss (Param 20 "p2")

testUO = createUnaryLoss cosOperator testC

testBO = createBinaryLoss plusOperator testUO testP1

testCompund1 = createUnaryLoss tanhOperator (createUnaryLoss cosOperator testP1)

testCompund2 = createUnaryLoss expOperator (createBinaryLoss plusOperator (createUnaryLoss cosOperator testP1) (createUnaryLoss sinOperator testP2))

testCompund = createBinaryLoss powerOperator (createCoefLoss 0.5) (createVarLoss (Param 3 "p"))

testSame = createUnaryLoss expOperator (createBinaryLoss plusOperator (createUnaryLoss cosOperator testP1) (createUnaryLoss sinOperator testP1))

-- FeedForward

feedforward :: Loss -> Loss
feedforward loss@(Coef constant _ _) = Coef constant (evaluate loss) 0.0
feedforward loss@(Var var _ _) = Var var (evaluate loss) 0.0
feedforward loss@(Binary f x1 x2 _ _) = Binary f (feedforward x1) (feedforward x2) (evaluate loss) 0.0
feedforward loss@(Unary f x _ _) = Unary f (feedforward x) (evaluate loss) 0.0

-- Zero Gradients

zeroGrad :: Loss -> Loss
zeroGrad (Coef constant v _) = Coef constant v 0.0
zeroGrad (Var var v _) = Var var v 0.0
zeroGrad (Binary f x1 x2 v _) = Binary f (zeroGrad x1) (zeroGrad x2) v 0.0
zeroGrad (Unary f x v _) = Unary f (zeroGrad x) v 0.0

-- Backpropagation

backpropagation :: Loss -> Loss
-- L = H ( G ( F ( x ) ) )
-- dL/dx = dH/dG . dG/dF . dF/dx
-- L = H ( M ( F, G) )
-- dL/dx = dH/dM . dM/dx = dH/dM . (dM/dF . dF/dx + dM/dG . dM/dx)
backpropagation loss = chainRuleStep loss Nothing
  where
    chainRuleStep :: Loss -> Maybe (Float, (Float -> Float)) -> Loss
    chainRuleStep loss@(Coef constant v _) parentData = (Coef constant v 0.0)
    chainRuleStep loss@(Var (Param param name) v _) parentData = case parentData of
      Nothing -> Var (Param param name) v 1.0
      Just (parentGradient, parentFunDerivative) -> Var (Param param name) v (parentGradient * parentFunDerivative v)
    chainRuleStep loss@(Unary f x v _) parentData = case parentData of
      Nothing -> Unary f (chainRuleStep x (Just (1.0, uDerivative f))) v 1.0
      Just (parentGradient, parentFunDerivative) -> Unary f (chainRuleStep x (Just (gradient, uDerivative f))) v gradient
        where
          gradient = (parentGradient * parentFunDerivative v)
    chainRuleStep loss@(Binary f x1 x2 v _) parentData = case parentData of
      Nothing -> Binary f (chainRuleStep x1 (Just (1.0, lDerivative))) (chainRuleStep x2 (Just (1.0, rDerivative))) v 1.0
      Just (parentGradient, parentFunDerivative) -> Binary f (chainRuleStep x1 (Just (gradient, lDerivative))) (chainRuleStep x2 (Just (gradient, rDerivative))) v gradient
        where
          gradient = (parentGradient * parentFunDerivative v)
      where
        lDerivative = leftDerivative f (value x1) (value x2)
        rDerivative = rightDerivative f (value x1) (value x2)

-- Loss gradient wrt parameter (after backprop)

gradient :: Loss -> Param -> Float
gradient loss param = addPartialGradient loss param 0.0
  where
    addPartialGradient (Coef constant v partialGradient) param totalGradient = totalGradient
    addPartialGradient (Var var v partialGradient) param totalGradient
      | var == param = totalGradient + partialGradient
      | otherwise = totalGradient
    addPartialGradient (Binary f x1 x2 v partialGradient) param totalGradient = (addPartialGradient x1 param totalGradient) + (addPartialGradient x2 param totalGradient)
    addPartialGradient (Unary f x v partialGradient) param totalGradient = addPartialGradient x param totalGradient
