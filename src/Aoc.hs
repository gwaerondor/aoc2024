module Aoc (
  solve,
  solve2,
  fileToLines,
  fileToLinesM,
  fileToLine,
  fileToCsv,
  fileToCsvM,
  fileToSections,
  prettyResult,
  toResult,
  choose,
  takeWhileInclusive,
  count,
  (<.>),
  (!!!),
  (!!?),
  Result (..)
) where

import Data.List.Split
import Data.List
import Control.Monad ((>=>))

data Result =
  Result String String
  deriving Show

toResult :: Show a => Show b => a -> b -> Result
toResult a b = Result (show a) (show b)

prettyResult :: Result -> String
prettyResult (Result a b) =
  "Part 1: " ++ a ++ "\nPart 2: " ++ b

-- Solve by applying f to the contents of the file and printing the result.
solve :: Show b => (a -> b) -> IO a -> IO String
solve f contents = fmap (show . f) contents

solve2 :: Show b => Show c => (a -> b) -> (a -> c) -> IO a -> IO Result
solve2 f g contents = do
  p1 <- solve f contents
  p2 <- solve g contents
  return $ toResult p1 p2

fileToLines :: FilePath -> IO [String]
fileToLines = readFile >=> (return . lines)

fileToLine :: FilePath -> IO String
fileToLine path = head <$> fileToLines path

fileToLineM :: (String -> a) -> FilePath -> IO a
fileToLineM f path = fmap f $ fileToLine path

fileToLinesM :: (String -> a) -> FilePath -> IO [a]
fileToLinesM f path = fmap f <$> fileToLines path

fileToCsv :: FilePath -> IO [String]
fileToCsv path = splitOn "," <$> readFile path

fileToCsvM :: (String -> a) -> FilePath -> IO [a]
fileToCsvM f path = fmap f <$> fileToCsv path

fileToSections :: FilePath -> IO [[String]]
fileToSections path = fmap lines <$> endBy "\n\n" <$> readFile path

(<.>) f g = (fmap f) . g

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs)
  | p x = x : takeWhileInclusive p xs
  | otherwise = [x]

choose :: (b -> Maybe a) -> [b] -> [a]
choose f (x:rest) = case f x of
                      Nothing -> choose f rest
                      Just y -> y : choose f rest
choose _ [] = []

count :: (a -> Bool) -> [a] -> Int
count p = length . (filter p)

(!!!) :: [[a]] -> (Int, Int) -> a
matrix !!! (x, y) =
  (matrix !! y) !! x

(!!?) :: [[a]] -> (Int, Int) -> Maybe a
matrix !!? (x, y) =
  matrix !? y >>= (!? x)
