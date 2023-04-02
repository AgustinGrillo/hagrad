module Lib (module Lib) where

import Loss
import Operators

-- Parameter creation
param :: Float -> String -> Loss
param value name = createVarLoss (Param value name)

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

