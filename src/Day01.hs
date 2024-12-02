module Day01 (run) where
import Aoc
import Data.List

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = transpose <$> fileToLinesM (read <.> words) inputFile

part1 :: [[Int]] -> Int
part1 [left, right] = sum . (fmap f) $ zip (sort left) (sort right)
  where f (x, y) = abs $ x - y

part2 :: [[Int]] -> Int
part2 [left, right] = sum $ similarityScore right <$> left
  where
    similarityScore right x = sum $ filter (== x) right
