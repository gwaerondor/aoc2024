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
  (<.>),
  Result (..)
) where

import Data.List.Split

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

solve2 :: Show b => (a -> b) -> (a -> b) -> IO a -> IO Result
solve2 f g contents = do
  p1 <- solve f contents
  p2 <- solve g contents
  return $ toResult p1 p2

fileToLines :: FilePath -> IO [String]
fileToLines path = do
  text <- readFile path
  return $ lines text

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
