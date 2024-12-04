module Day04 (run) where
import Aoc
import Data.Universe.Helpers (diagonals)
import Data.List (transpose, isPrefixOf)
import Control.Applicative (liftA2)

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

(!!!) matrix (x, y) =
  (matrix !! y) !! x

part2 :: [[Char]] -> Int
part2 lines =
  let xs = [1..((length $ (head lines)) - 2)]
      ys = [1..((length $ lines) - 2)]
      coords = [ (x, y) | x <- xs, y <- ys ]
  in
    length $ filter (f lines) coords
    where
      f ls cs = all (`elem` ["MAS", "SAM"]) [(rise cs ls), (fall cs ls)]
      rise (x, y) lines = (lines !!!) <$> zip [x-1, x, x+1] [y+1, y, y-1]
      fall (x, y) lines = (lines !!!) <$> zip [x-1, x, x+1] [y-1, y, y+1]
