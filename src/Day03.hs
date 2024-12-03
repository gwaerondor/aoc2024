module Day03 (run) where
import Aoc
import Data.Maybe (catMaybes)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)

data Instruction =
  Mul Int Int
  | Do
  | Don't

run :: FilePath -> IO Result
run inputFile =
  solve2 part1 part2 contents
  where
    contents = catMaybes . parse <$> readFile inputFile

part1 :: [Instruction] -> Int
part1 input = sum $ f <$> input
  where f (Mul a b) = a * b
        f _ = 0

part2 :: [Instruction] -> Int
part2 = f Do
  where f _ (Don't : more) = f Don't more
        f _ (Do : more) = f Do more
        f Do ((Mul a b) : rest) = (a * b) + (f Do rest)
        f Don't (_ : rest) = f Don't rest
        f _ [] = 0

parse :: String -> [Maybe Instruction]
parse [] = []
parse s
  | "do()" `isPrefixOf` s = (Just Do) : (parse $ drop 4 s)
  | "don't()" `isPrefixOf` s = (Just Don't) : (parse $ tail s)
  | "mul" `isPrefixOf` s = (tryParseArguments $ drop 3 s) : (parse $ tail s)
  | otherwise = parse $ tail s

tryParseArguments :: String -> Maybe Instruction
tryParseArguments s =
  let nextToken = takeWhileInclusive (/= ')') s in
    f nextToken
  where
    f token
      | isValid token =
        case splitOn "," $ filter (\c -> not $ c `elem` "()") token of
          [a, b] -> Just $ Mul (read a) (read b)
          _ -> Nothing
      | otherwise = Nothing
    isValid x =
      "(" `isPrefixOf` x
      && ")" `isSuffixOf` x
      && count (== ',') x == 1 -- Allows (,1) but that doesn't happen so it's OK
      && all (`elem` "0123456789,()") x
