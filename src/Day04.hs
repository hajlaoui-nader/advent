module Day04 where

import Data.Char
import Data.Either
import Data.List

input :: [Int]
input = [136760 .. 595730]

type Password = [Int]
data PasswordError = WrongDigits | NotAdjacent | Decrease | NoDouble deriving Show

digits :: Int -> Password
digits = map digitToInt . show

validate :: Password -> Either PasswordError Password
validate pwd = do
    p1 <- sixDigits pwd
    p2 <- increaseOnly p1
    adjacent p2

sixDigits :: Password -> Either PasswordError Password
sixDigits pwd = if length pwd == 6 then Right pwd else Left WrongDigits

increaseOnly :: Password -> Either PasswordError Password
increaseOnly pwd = if fst scan then Right pwd else Left Decrease
    where
        scan = foldl func (True, 0) pwd
        func state lastdigit = (fst state && snd state <= lastdigit, lastdigit)

adjacent :: Password -> Either PasswordError Password
adjacent pwd = if fst scan then Right pwd else Left NotAdjacent
    where
        scan = foldl func (False, 0) pwd
        func state lastdigit = (fst state || snd state == lastdigit, lastdigit)

containsDouble :: Password -> Either PasswordError Password
containsDouble pwd = if bool then Right pwd else Left NoDouble
    where
        bool = any (\x -> length x == 2) $ group pwd

day04 :: String -> String
day04 _ = show . length . filter isRight . map validate . map digits $ input

day04b :: String -> String
day04b _ = show . length . filter isRight . map validate2 . map digits $ input

validate2 :: Password -> Either PasswordError Password
validate2 pwd = do
    p1 <- sixDigits pwd
    p2 <- increaseOnly p1
    p3 <- adjacent p2
    containsDouble p3
