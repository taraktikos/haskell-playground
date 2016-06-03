module Lib where

someFunc :: IO ()
someFunc = putStrLn "Hello world"

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a + b + c == 24, a^2 + b^2 == c^2]

circumference :: Float -> Float
circumference r = 2 * pi * r

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + y1, x2 + y2)

head' :: [a] -> a
head' [] = error "Error"
head' (x:_) = x
