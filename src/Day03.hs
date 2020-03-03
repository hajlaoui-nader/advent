{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import qualified Data.Text as T
import           Control.Applicative
import           Data.Attoparsec.Text
import           Prelude                    hiding (readFile, take)
import Data.Maybe

data Direction = Up | Down | Rightd | Leftd deriving (Eq, Read, Show)

data Point = Point Int Int deriving (Eq, Show)
data Instruction = Instruction Direction Int deriving (Read, Eq, Show)
data WireDefinition = WireDefinition [Instruction] deriving (Eq, Show)

input :: String -> [WireDefinition]
input str = catMaybes maybeDefinitions 
    where maybeDefinitions = fmap wireDefinitionInput . lines $ str

wireDefinitionInput :: String -> Maybe WireDefinition
wireDefinitionInput str = case instructions $ T.pack str of
                            Nothing -> Nothing
                            Just [] -> Nothing
                            Just x -> Just (WireDefinition x)

directionParser :: Parser Direction
directionParser = (string "U"    >> return Up)
 <|> (string "D" >> return Down)
 <|> (string "R"  >> return Rightd)
 <|> (string "L" >> return Leftd)


instructionParser :: Parser Instruction
instructionParser = do
    dir <- directionParser
    Instruction dir <$> decimal

instructionsParser :: Parser [Instruction]
instructionsParser = instructionParser `sepBy` (char ',')

instructions :: T.Text -> Maybe [Instruction]
instructions str = toMaybe $ parseOnly instructionsParser str 

main :: IO ()
main = print $ toMaybe $ parseOnly instructionsParser "U987,D99"

toMaybe :: Either b a -> Maybe a
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

line :: WireDefinition -> [Point]
line = undefined 