module Lib (module Lib)
    where

tester :: IO ()
tester = putStrLn $ "test"

--- Custom Data Types
data Param = Param Float

data Loss = Op String Loss Loss | Var Param | Coef Float 

evaluate :: Loss -> Float
evaluate (Var (Param param)) = param
evaluate (Op "*" a b) = (evaluate a) * (evaluate b)

