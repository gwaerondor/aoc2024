module Day04 (run) where
import Aoc
import Data.Universe.Helpers (diagonals)
import Data.List (transpose, isPrefixOf)

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = fileToLines inputFile

part1 :: [[Char]] -> Int
part1 lines =
  let possibilities = lines ++ (transpose lines) ++ (diagonals lines) ++ (diagonals . reverse $ lines) in
    sum $ f <$> possibilities ++ (reverse <$> possibilities)
    where
      f [] = 0
      f s
        | "XMAS" `isPrefixOf` s = 1 + (f $ tail s)
        | otherwise = 0 + (f $ tail s)

(!!!) matrix (y, x) =
  (matrix !! y) !! x

part2 :: [[Char]] -> Int
part2 lines =
  let coords = [ (x, y) | y <- [1..((length $ lines) - 2)], x <- [1..((length $ (head lines)) - 2)] ] in
    length $ filter (f lines) coords
    where
      f ls cs = all (`elem` ["MAS", "SAM"]) [(rise cs ls), (fall cs ls)]
      rise (x, y) lines = [lines !!! (x-1, y+1), lines !!! (x, y), lines !!! (x+1, y-1)]
      fall (x, y) lines = [lines !!! (x-1, y-1), lines !!! (x, y), lines !!! (x+1, y+1)]
