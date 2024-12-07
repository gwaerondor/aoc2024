module Day06 (run) where
import Aoc
import Data.List (intercalate, nub)
import Data.Function ((&))
import Control.Monad.Trans.State
import Data.Maybe (isNothing)

test = [
  "....#.....",
  ".........#",
  "..........",
  "..#.......",
  ".......#..",
  "..........",
  ".#..^.....",
  "........#.",
  "#.........",
  "......#..."
  ] & intercalate "\n"

type Coord = (Int, Int)

data S = S { _you :: You,
             _tileMap :: [[Tile]],
             _visited :: [Coord] }
       deriving Show

data Facing =
  North
  | East
  | West
  | South
  deriving Show

clockwise :: Facing -> Facing
clockwise North = East
clockwise East = South
clockwise South = West
clockwise West = North

northOf (x, y) = (x, y-1)
southOf (x, y) = (x, y+1)
eastOf (x, y) = (x+1, y)
westOf (x, y) = (x-1, y)

data You = You { _facing :: Facing, _coord :: Coord } deriving Show

data Tile =
  Wall
  | Empty
  deriving (Show, Eq)

parse :: [String] -> [[Tile]]
parse lines =
  fmap p <$> lines
  where p '#' = Wall
        p _ = Empty

part1 :: [[Tile]] -> Int -- -> S
part1 x =
  let initialS =
        S { _you = You { _facing = North, _coord = (50, 45) }, -- Just checked column and line count in input
            _tileMap = x,
            _visited = []
          }
  in
    length . nub . _visited . snd $ runState f initialS
  where
    f = do
          tick
          s <- get
          if isOutOfBounds s then
            return ()
          else f

isOutOfBounds :: S -> Bool
isOutOfBounds s =
  isNothing $ _tileMap s !!? (_coord $ _you s)

tick :: State S ()
tick = do
  s <- get
  let withLoc = s { _visited = (_coord $ _you s) : _visited s } in
    if nextSquare s == Just Wall then
      let you = _you s in
        put $ withLoc { _you = you { _facing = clockwise $ _facing you } }
    else
      put $ withLoc { _you = step (_you s) }
  where
    nextSquare s =
      let you = _you s
          yourLocation = _coord you
      in
        case _facing you of
          North -> _tileMap s !!? (northOf $ _coord you)
          South -> _tileMap s !!? (southOf $ _coord you)
          East -> _tileMap s !!? (eastOf $ _coord you)
          West -> _tileMap s !!? (westOf $ _coord you)

step :: You -> You
step you =
  let (x, y) = _coord you in
    case _facing you of
      North -> you { _coord = northOf $ _coord you }
      East -> you { _coord = eastOf $ _coord you }
      South -> you { _coord = southOf $ _coord you }
      West -> you { _coord = westOf $ _coord you }

run :: FilePath -> IO Result
run inputFile = solve2 part1 f contents
  where
    contents = parse <$> fileToLines inputFile
    --contents = return . parse . lines $ test
    f _ = "Not done"
