module Lib (tester, add) where

add :: (Num a) => a -> a -> a
add x y = x + y

tester :: IO ()
tester = putStrLn $ show $ (add 2 2)

