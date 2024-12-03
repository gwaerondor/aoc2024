module Day03 (run) where
import Aoc
import Data.Maybe (isJust)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)

data Instruction =
  Mul Int Int
  | Do
  | Dont
  deriving Show

run :: FilePath -> IO Result
run inputFile =
  solve2 part1 part2 contents
  where
    contents = readFile inputFile

part1 :: String -> Int
part1 input = sum $ f <$> parse input
  where f (Mul a b) = a * b
        f _ = 0

part2 :: String -> Int
part2 input = (f Do) $ parse input
  where f _ (Dont : more) = f Dont more
        f _ (Do : more) = f Do more
        f Do ((Mul a b) : rest) = (a * b) + (f Do rest)
        f Dont (_ : rest) = f Dont rest
        f _ [] = 0

parse = (choose id) . parseTopLevel

parseTopLevel :: String -> [Maybe Instruction]
parseTopLevel [] = []
parseTopLevel s
  | "do()" `isPrefixOf` s = (Just Do) : (parseTopLevel $ drop 4 s)
  | "don't()" `isPrefixOf` s = (Just Dont) : (parseTopLevel $ tail s)
  | "mul" `isPrefixOf` s = (tryParseArguments $ drop 3 s) : (parseTopLevel $ tail s)
  | otherwise = parseTopLevel $ tail s

tryParseArguments :: String -> Maybe Instruction
tryParseArguments s =
  let nextToken = takeWhileInclusive (/= ')') s in
    case isValid nextToken of
      True ->
        case splitOn "," $ filter (\c -> not $ c `elem` "()") nextToken of
          [a, b] -> Just $ Mul (read a) (read b)
          _ -> Nothing
      False -> Nothing
  where
    isValid x =
      "(" `isPrefixOf` x
      && ")" `isSuffixOf` x
      && count (== ',') x == 1 -- Allows (,1) but that doesn't happen so it's OK
      && all (`elem` "0123456789,()") x
