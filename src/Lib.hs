module Lib where

someFunc :: IO ()
someFunc = putStrLn "Hello world"

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a + b + c == 24, a^2 + b^2 == c^2]

circumference :: Float -> Float
circumference r = 2 * pi * r
