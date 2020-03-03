{-# LANGUAGE OverloadedStrings #-}
module Day03 where

import qualified Data.Text as T
import           Control.Applicative
import           Data.Attoparsec.Text
import           Prelude                    hiding (readFile, take)

data Direction = Up | Down | Rightd | Leftd deriving (Eq, Read, Show)

data Point = Point Int Int deriving (Eq, Show)
data Instruction = Instruction Direction Int deriving (Read, Eq, Show)
data WireDefinition = WireDefinition [Instruction] deriving (Eq, Show)

-- instance Read WireDefinition where 
--   read str = undefined

-- input :: String -> [WireDefinition]
input = fmap wireDefinitionInput . lines

wireDefinitionInput :: String -> Maybe WireDefinition
wireDefinitionInput str = undefined

directionParser :: Parser Direction
directionParser = (string "U"    >> return Up)
 <|> (string "D" >> return Down)
 <|> (string "R"  >> return Rightd)
 <|> (string "L" >> return Leftd)


instructionParser :: Parser Instruction
instructionParser = do
    dir <- directionParser
    Instruction dir <$> decimal
    
main :: IO ()
main = print $ toMaybe $ parseOnly instructionParser "U987"

toMaybe :: Either b a -> Maybe a
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

-- main2 :: IO ()
-- main2 = print $ (maybeResult  parse)  "U987"