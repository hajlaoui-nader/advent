{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import qualified Data.Text as T
import           Control.Applicative
import           Data.Attoparsec.Text
import           Prelude                    hiding (readFile, take)
import           Data.Maybe
import           Data.List
import qualified Data.Set as S
import qualified Data.Map as M

data Direction = Up | Down | Rightd | Leftd deriving (Eq, Read, Show, Ord)

data Point = Point Int Int deriving (Eq, Show, Ord)
data Instruction = Instruction Direction Int deriving (Read, Eq, Show, Ord)
data WireDefinition = WireDefinition [Instruction] deriving (Eq, Show, Ord)

input :: String -> S.Set WireDefinition
input str = S.fromList $ catMaybes maybeDefinitions
    where maybeDefinitions = fmap wireDefinitionInput $ lines $ str

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

day03 :: String -> String
day03 = show . minimum . manhattanDistance . collisionPoints

toMaybe :: Either b a -> Maybe a
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x


wirepoints :: WireDefinition -> S.Set Point
wirepoints (WireDefinition instrs) = S.fromList $ unfoldr recur (instrs, initialPoint)
 where recur ([], _) = Nothing
       recur ((Instruction Up 0):rest, p) = recur (rest, p)
       recur ((Instruction Up n):rest, (Point x y)) = Just ((Point x (y+1)), (((Instruction Up (n - 1)):rest), (Point x (y+1))))
       recur ((Instruction Down 0):rest, p) = recur (rest, p)
       recur ((Instruction Down n):rest, (Point x y)) = Just ((Point x (y-1)), (((Instruction Down (n - 1)):rest), (Point x (y-1))))
       recur ((Instruction Leftd 0):rest, p) = recur (rest, p)
       recur ((Instruction Leftd n):rest, (Point x y)) = Just ((Point (x-1) y), (((Instruction Leftd (n - 1)):rest), (Point (x-1) y)))
       recur ((Instruction Rightd 0):rest, p) = recur (rest, p)
       recur ((Instruction Rightd n):rest, (Point x y)) = Just ((Point (x+1) y), (((Instruction Rightd (n - 1)):rest), (Point (x+1) y)))

initialPoint :: Point
initialPoint = Point 0 0


collisionPoints :: String -> S.Set Point
collisionPoints sr = S.filter negativePoints collisions
    where collisions = intersection $ S.map wirepoints $ input $ sr

manhattanDistance :: S.Set Point -> S.Set Int
manhattanDistance = S.map (dist initialPoint)
        where dist (Point x y) (Point x' y') = abs (x - x') + abs (y - y')

negativePoints :: Point -> Bool
negativePoints = (/=) initialPoint

intersection:: (Eq a, Ord a) => S.Set (S.Set a) -> S.Set a
intersection = foldr1 S.intersection

day03b :: String -> String
day03b = show . minimum . combinedSteps

-- combinedSteps :: String -> S.Set Int
combinedSteps str = S.map (steps wpts) $ collision
    where wpts = S.map stepsWirepoints $ input $ str
          steps points point = sum $ S.map (M.findWithDefault 0 point) $ points
          collision = collisionPoints str

stepsWirepoints :: WireDefinition -> M.Map Point Int
stepsWirepoints (WireDefinition instrs) = M.fromList $ unfoldr recur (instrs, initialPoint, 0)
 where recur ([], _, _) = Nothing
       recur (Instruction Up 0:rest, p, s) = recur (rest, p, s)
       recur (Instruction Up n:rest, Point x y, s) = Just ((Point x (y+1), s+1), (Instruction Up (n - 1):rest, Point x (y+1), s+1))
       recur (Instruction Down 0:rest, p, s) = recur (rest, p, s)
       recur (Instruction Down n:rest, Point x y, s) = Just ((Point x (y-1), s+1), (Instruction Down (n - 1):rest, Point x (y-1), s+1))
       recur (Instruction Leftd 0:rest, p, s) = recur (rest, p, s)
       recur (Instruction Leftd n:rest, Point x y, s) = Just ((Point (x-1) y, s+1), (Instruction Leftd (n - 1):rest, Point (x-1) y, s+1))
       recur (Instruction Rightd 0:rest, p, s) = recur (rest, p, s)
       recur (Instruction Rightd n:rest, Point x y, s) = Just ((Point (x+1) y, s+1), (Instruction Rightd (n - 1):rest, Point (x+1) y, s+1))