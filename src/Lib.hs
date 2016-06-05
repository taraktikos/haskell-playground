module Lib where

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where
      bmi weight height = weight / height ^ 2

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where
      (f:_) = firstName
      (l:_) = lastName

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= skinny = "Small"
    | bmi <= normal = "Normal"
    | otherwise = "Fat"
    where
      bmi = weight / height ^ 2
      (skinny, normal) = (18.5, 25.0)

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "Small"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Big"
    | otherwise = "Fat"

firstLetter :: String -> String
firstLetter "" = "Empty"
firstLetter all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "1 element: " ++ show x
tell (x:y:[]) = "2 elements: " ++ show x ++ " " ++ show y
tell (x:y:_) = "More than 2 elements"

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
