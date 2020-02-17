module Day02 where

import Data.List.Split
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe 


type IntMap = IM.IntMap Int 
type ArithmeticFunction = Int -> Int -> Int

type Noun = Int
type Verb = Int
data OpCode = Halt | Error | Add | Mult deriving Show


input :: String -> [Int]
input = fmap read . endBy ","

indexed :: String -> IntMap
indexed x = IM.insert 2 2 $ IM.insert 1 12 $ IM.fromList . zip [0 ..] $ input x

run :: IntMap -> Maybe IntMap
run = run' 0
    where run' index intmap = case opCode (valueAtIndex index intmap) of 
                                Halt -> Just intmap
                                Error -> Nothing
                                Add -> run' (index + 4) $ compute index (+) intmap
                                Mult -> run' (index + 4) $ compute index (*) intmap

opCode :: Int -> OpCode
opCode x = case x of
    1 -> Add
    2 -> Mult
    99 -> Halt
    _ -> Error

compute :: Int -> ArithmeticFunction -> IntMap -> IntMap
compute index func intmap = IM.insert position3 (func value1 value2) intmap
    where position1 = valueAtIndex (index + 1) intmap
          position2 = valueAtIndex (index + 2) intmap
          position3 = valueAtIndex (index + 3) intmap
          value1 = valueAtIndex position1 intmap
          value2 = valueAtIndex position2 intmap


valueAtIndex :: Int -> IntMap -> Int
valueAtIndex pos im = fromMaybe 0 (IM.lookup pos im)

day02 :: String -> String
day02 s = show $ evaluate (indexed s)

evaluate :: IntMap -> Int
evaluate intmap = head $ fmap snd $ IM.toList $ fromJust $ run intmap

nouns :: [Noun]
nouns = [0..9]

verbs :: [Verb]
verbs = [0..9]

iterateIM :: IntMap -> [((Noun, Verb), Int)]
iterateIM intmap = catMaybes $ do 
    n <- nouns
    v <- verbs
    let intmap' = IM.insert 2 v $ IM.insert 1 n intmap
    pure $ sequenceA ((n, v), Just (evaluate intmap'))

outputToProduce = 19690720

findNounAndVerb :: [((Noun, Verb), Int)] -> (Noun, Verb)
findNounAndVerb = fst . head . filter (\(_, result) -> result == outputToProduce)

nounAndVerb s = findNounAndVerb $ iterateIM (indexed s)

day02b :: String -> String
day02b s = show $ 100 * fst nv + snd nv
  where nv = nounAndVerb s
