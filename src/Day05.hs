module Day05 (run) where
import Aoc
import Data.List (intercalate, intersect)
import Data.List.Split (endBy, splitOn)
import Data.Function ((&))

data OrderingRule = OrderingRule Int Int deriving Show
data Input = Input [OrderingRule] [[Int]] deriving Show

part1 :: Input -> Int
part1 (Input orderingRules sequences) =
  sum $ middleValue <$> filter (\s -> sequenceIsValid [] (head s) (tail s) orderingRules) sequences

middleValue :: [Int] -> Int
middleValue ls = ls !! ((length ls) `div` 2)

isLT :: Int -> OrderingRule -> Bool
isLT n (OrderingRule x _)
  | n == x = True
  | otherwise = False

isGT :: Int -> OrderingRule -> Bool
isGT n (OrderingRule _ x)
  | n == x = True
  | otherwise = False

getLow :: OrderingRule -> Int
getLow (OrderingRule x _) = x
getHigh :: OrderingRule -> Int
getHigh (OrderingRule _ x) = x

sequenceIsValid :: [Int] -> Int -> [Int] -> [OrderingRule] -> Bool
sequenceIsValid before current after rules =
  let allowedHigher = getHigh <$> filter (isLT current) rules
      allowedLower = getLow <$> filter (isGT current) rules
  in
    if intersect allowedLower after == [] && intersect allowedHigher before == [] then
      if after == [] then True
      else
        sequenceIsValid (current : before) (head after) (tail after) rules
    else
      False

parseOrderingRule :: String -> OrderingRule
parseOrderingRule s =
  case splitOn "|" s of
    [low, high] -> OrderingRule (read low) (read high)
    other -> error $ "Invalid ordering rule: " ++ (show other)

parse :: [[String]] -> Input
parse [rules, sequences] = Input (parseOrderingRule <$> rules) (fmap read <$> splitOn "," <$> sequences)
parse other = error $ "Couldn't parse " ++ (show other)

test =
  [
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  ] & intercalate "\n"

run :: FilePath -> IO Result
run inputFile = solve2 part1 f (parse <$> contents)
  where
    --testInput = return $ lines <$> endBy "\n\n" test
    contents = fileToSections inputFile
    f _ = "Not done"
