module Functions where

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where
      isLong xs = length xs > 15

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
        | even n = n:chain (n `div` 2)
        | odd n = n:chain (n * 3 + 1)

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
    where
      p x = x `mod` 3829 == 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = f y x

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where
  g x y = f y x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
