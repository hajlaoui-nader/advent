module Day01 where

input :: String -> [Int]
input = fmap read . lines

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

totalFuel :: [Int] -> Int
totalFuel = foldr ((+) . fuel) 0

day01 :: String -> String
day01 = show . totalFuel . input
