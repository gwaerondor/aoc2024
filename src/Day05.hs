module Day05 (run) where
import Aoc
import Data.List (intercalate, intersect, sortBy, find)
import Data.List.Split (endBy, splitOn)
import Data.Function ((&))
import Data.Maybe (catMaybes, isJust)

data OrderingRule = OrderingRule Int Int deriving (Show, Eq)
data Input = Input [OrderingRule] [[Int]] deriving (Show, Eq)

part1 :: Input -> Int
part1 (Input orderingRules sequences) =
  sum $ middleValue <$> filter (sequenceIsValid orderingRules) sequences

sequenceIsValid :: [OrderingRule] -> [Int] -> Bool
sequenceIsValid r s = s == (sort r s)

part2 :: Input -> Int
part2 (Input orderingRules sequences) =
  let incorrectSequences = filter (not . (sequenceIsValid orderingRules)) $ sequences
  in
    sum $ middleValue <$> sort orderingRules <$> incorrectSequences

sort :: [OrderingRule] -> [Int] -> [Int]
sort rules ns = sortBy ordering ns
  where
    ordering a b =
      case find isJust $ maybeOrder a b <$> rules of
        Just (Just ord) -> ord
        _ -> LT

maybeOrder :: Int -> Int -> OrderingRule -> Maybe Ordering
maybeOrder x y (OrderingRule a b)
  | a == x && b == y = Just LT
  | a == y && b == x = Just GT
  | otherwise = Nothing

middleValue :: [Int] -> Int
middleValue ls = ls !! ((length ls) `div` 2)

parseOrderingRule :: String -> OrderingRule
parseOrderingRule s =
  case splitOn "|" s of
    [low, high] -> OrderingRule (read low) (read high)
    other -> error $ "Invalid ordering rule: " ++ (show other)

parse :: [[String]] -> Input
parse [rules, sequences] = Input (parseOrderingRule <$> rules) (fmap read <$> splitOn "," <$> sequences)
parse other = error $ "Couldn't parse " ++ (show other)

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = parse <$> fileToSections inputFile

-- Kept from the original solution to part 1, when I naïvely thought I wouldn't
-- have to actually sort the list.
--
-- isLT :: Int -> OrderingRule -> Bool
-- isLT n (OrderingRule x _)
--   | n == x = True
--   | otherwise = False

-- isGT :: Int -> OrderingRule -> Bool
-- isGT n (OrderingRule _ x)
--   | n == x = True
--   | otherwise = False
--
-- getLow :: OrderingRule -> Int
-- getLow (OrderingRule x _) = x
-- getHigh :: OrderingRule -> Int
-- getHigh (OrderingRule _ x) = x
--
-- sequenceIsValid :: [Int] -> [OrderingRule] -> Bool
-- sequenceIsValid sequence = sequenceIsValid' [] (head sequence) (tail sequence)

-- sequenceIsValid' :: [Int] -> Int -> [Int] -> [OrderingRule] -> Bool
-- sequenceIsValid' before current after rules =
--   let allowedHigher = getHigh <$> filter (isLT current) rules
--       allowedLower = getLow <$> filter (isGT current) rules
--   in
--     if intersect allowedLower after == [] && intersect allowedHigher before == [] then
--       if after == [] then True
--       else
--         sequenceIsValid' (current : before) (head after) (tail after) rules
--     else
--       False
