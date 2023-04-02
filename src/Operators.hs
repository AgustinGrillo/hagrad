module Operators (module Operators) where

--- Supported operations

--- Binary Operators

data BinOp = BinOp {bName :: String, bOperator :: (Float -> Float -> Float), leftDerivative :: (Float -> Float -> (Float -> Float)), rightDerivative :: (Float -> Float -> (Float -> Float))}

plusOperator :: BinOp
plusOperator = BinOp "+" (+) (\_ _ -> const 1) (\_ _ -> const 1) -- d(f+g)/df = 1   d(f+g)/dg = 1

minusOperator :: BinOp
minusOperator = BinOp "-" (-) (\_ _ -> const 1) (\_ _ -> const (-1)) -- d(f-g)/df = 1   d(f-g)/dg = -1

divOperator :: BinOp
divOperator = BinOp "/" (/) (\_ g -> const (1 / g)) (\f g -> const (-f / (g ** 2))) -- d(f/g)/df = 1/g   d(f/g)/dg = -f/g^2

multOperator :: BinOp
multOperator = BinOp "*" (*) (\_ g -> const g) (\f _ -> const f) -- d(f.g)/df = g   d(f.g)/dg = f

powerOperator :: BinOp
powerOperator = BinOp "^" (**) (\f g -> const (g * f ** (g - 1))) (\f g -> const ((log f) * f ** (g))) -- d(f^g)/df = g.f^(g-1)   d(f^g)/dg = (f^g).ln(f)

--- Functoins and Unary Operators

data UnOp = UnOp {uName :: String, uOperator :: (Float -> Float), uDerivative :: (Float -> Float)}

cosOperator :: UnOp
cosOperator = UnOp "cos" cos ((* (-1)) . sin)

sinOperator :: UnOp
sinOperator = UnOp "sin" sin cos

tanOperator :: UnOp
tanOperator = UnOp "tan" tan ((1 /) . (** 2) . cos)

expOperator :: UnOp
expOperator = UnOp "exp" exp exp

tanhOperator :: UnOp
tanhOperator = UnOp "tanh" tanh ((1 /) . (** 2) . cosh)
