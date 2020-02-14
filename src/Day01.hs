module Day01 where

input :: String -> [Int]
input = fmap read . lines

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

totalFuel :: [Int] -> Int
totalFuel = foldr ((+) . fuel) 0

day01 :: String -> String
day01 = show . totalFuel . input

fuel' :: Int -> Int
fuel' = sum . tail . takeWhile (> 0) . iterate fuel

totalFuel' :: [Int] -> Int
totalFuel' = foldr ((+) . fuel') 0

day01b :: String -> String
day01b = show . totalFuel' . input