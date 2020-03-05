module AdventOfCode where

import Day01
import Day02
import Day03
import Day04

inputFp :: String -> FilePath
inputFp n = "resources/Day" ++ n ++ ".txt"

argLookup :: [(String, String -> String)]
argLookup = [ ("01" , day01)
            , ("01b", day01b)
            , ("02", day02)
            , ("02b", day02b)
            , ("03", day03)
            , ("03b", day03b)
            , ("04", day04)
         ]
