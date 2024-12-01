module Day01 (run) where
import Aoc
import Data.List

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = fileToLinesM (read <.> words) inputFile

part1 :: [[Int]] -> Int
part1 pairs = pairwiseEuclidianDistance . transpose $ pairs

pairwiseEuclidianDistance [a, b] = sum . (fmap f) $ zip (sort a) (sort b)
  where f (x, y) = abs $ x - y
pairwiseEuclidianDistance _ = -1

part2 :: [[Int]] -> Int
part2 pairs = f . transpose $ pairs
  where
    f [left, right] = sum $ similarityScore right <$> left
    similarityScore right x = x * (count right x)
    count right x = length $ filter (== x) right
