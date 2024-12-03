module Day03 (run) where
import Aoc
import Data.Maybe (isJust)
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
part1 input = sum . fmap f . parse $ input
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
parseTopLevel ('d':'o':'(':')':rest) = (Just Do) : (parseTopLevel rest)
parseTopLevel ('d':'o':'n':'\'':'t':'(':')':rest) = (Just Dont) : (parseTopLevel rest)
parseTopLevel ('m':'u':'l':rest) = (tryParseArguments rest) : parseTopLevel rest
parseTopLevel (_:rest) = parseTopLevel rest
parseTopLevel [] = []

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
    beginsWith c s = case s of c : _ -> True
                               _ -> False
    endsWith c = (beginsWith c) . reverse
    isValid x =
      beginsWith '(' x
      && endsWith ')' x
      && count (== ',') x == 1
      && (all (\c -> c `elem` "0123456789,()")) x
