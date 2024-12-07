module Day07 (run) where
import Aoc
import Data.Function ((&))
import Data.Char (isNumber)
import Data.List (intercalate)

data Equation = Equation Int [Int] deriving Show

part1 :: [Equation] -> Int
part1 = sumValid [(+), (*)]

part2 :: [Equation] -> Int
part2 = sumValid [(+), (*), (⌢)]

sumValid :: [(Int -> Int -> Int)] -> [Equation] -> Int
sumValid ops es =
  sum $ getTarget <$> filter (\(Equation target terms) -> isValid terms target ops 0) es

isValid :: [Int] -> Int -> [(Int -> Int -> Int)] -> Int -> Bool
isValid [] target _ acc = acc == target
isValid (t:terms) target ops acc
  | acc > target = False
  | otherwise = any (\op -> isValid terms target ops (op acc t)) ops

getTarget :: Equation -> Int
getTarget (Equation t _) = t

(⌢) :: Int -> Int -> Int
a ⌢ b = read $ (show a) ++ (show b)

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = fmap parse <$> fileToLines inputFile

parse :: String -> Equation
parse ln =
  let (res : terms) = read <$> filter isNumber <$> words ln in
    Equation res terms
