module AdventOfCode where

import Day01
import Day02

inputFp :: String -> FilePath
inputFp n = "resources/Day" ++ n ++ ".txt"

argLookup :: [(String, String -> String)]
argLookup = [ ("01" , day01)
            , ("01b", day01b)
            , ("02", day02)
         ]
