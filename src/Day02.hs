module Day02 (run) where
import Aoc
import Data.List

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = fileToLinesM (read <.> words) inputFile

count f = length . (filter f)

part1 :: [[Int]] -> Int
part1 = count reportIsSafe

part2 :: [[Int]] -> Int
part2 = count reportIsSafe2

reportIsSafe :: [Int] -> Bool
reportIsSafe report = (sort report `elem` [report, reverse report]) && f report
  where
    f (a : b : rest) = (step a b) >= 1 && (step a b) <= 3 && f (b : rest)
    f _ = True
    step a b = abs (a - b)

reportIsSafe2 :: [Int] -> Bool
reportIsSafe2 = any reportIsSafe . allPossibilities
  where
    withoutIx ls n = take n ls ++ drop (n + 1) ls
    allPossibilities ls = withoutIx ls <$> [0..(length ls - 1)]
