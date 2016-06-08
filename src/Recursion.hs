module Recursion where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smSorted = quicksort [a | a <- xs, a <= x]
        bgSorted = quicksort [a | a <- xs, a > x]
    in smSorted ++ [x] ++ bgSorted

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
