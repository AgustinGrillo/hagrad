module Lib (module Lib)
    where

tester :: IO ()
tester = putStrLn $ "test"

--- Custom Data Types
type UnOp = (Float -> Float)
type BinOp = (Float -> Float -> Float)
data Param = Param Float

data Loss = Unary UnOp Loss | Binary BinOp Loss Loss | Var Param | Coef Float 

evaluate :: Loss -> Float
evaluate (Coef constant) = constant
evaluate (Var (Param param)) = param
evaluate (Binary f x1 x2) = f (evaluate x1) (evaluate x2)
evaluate (Unary f x) = f (evaluate x)

