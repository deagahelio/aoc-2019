module Day3 where

import Data.List (find, minimum)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace
import Prelude hiding (Left, Right)
import Utils (tail', wordsWhen)

data Direction
  = Up
  | Right
  | Down
  | Left

instance Show Direction where
  show dir =
    case dir of
      Up    -> "U"
      Right -> "R"
      Down  -> "D"
      Left  -> "L"

data Move =
  Move
    { dir :: Direction
    , dist :: Int
    }

instance Show Move where
  show move = show (dir move) ++ show (dist move)

data Coords =
  Coords
    { x :: Int
    , y :: Int
    }
  deriving (Eq, Ord)

instance Show Coords where
  show coords = "(" ++ show (x coords) ++ ", " ++ show (y coords) ++ ")"

(<+>) :: Coords -> Coords -> Coords
(Coords ax ay) <+> (Coords bx by) = Coords (ax + bx) (ay + by)

data Step =
  Step
    { idx :: Int
    , coords :: Coords
    }

instance Show Step where
  show step = show (idx step) ++ ": " ++ show (coords step)

parseMove :: String -> Move
parseMove s =
  let direction =
        case head s of
          'U' -> Up
          'R' -> Right
          'D' -> Down
          'L' -> Left
      distance = read (tail s)
   in Move { dir = direction, dist = distance }

moveToCoords :: Move -> Coords
moveToCoords move =
  case dir move of
    Up    -> Coords 0 (-dist move)
    Right -> Coords (dist move) 0
    Down  -> Coords 0 (dist move)
    Left  -> Coords (-dist move) 0

intermediateCoords :: Coords -> [Coords]
intermediateCoords coords =
  case coords of
    Coords x 0 -> map (flip Coords 0) [0,signum x..x]
    Coords 0 y -> map (Coords 0) [0,signum y..y]
    _ -> error "x or y must be zero"

coordsToSteps :: [Coords] -> [Step]
coordsToSteps = zipWith Step [0..]

tracePath :: Coords -> [Move] -> [Coords]
tracePath _ [] = []
tracePath origin (move:moves) =
  let coords = moveToCoords move
      next = coords <+> origin
  in map (<+> origin) (intermediateCoords coords) ++ tail' (tracePath next moves)

takeShortcuts :: [Step] -> [Step]
takeShortcuts steps =
  let loop (step:steps) counter indexMap =
        let (newStep, newIndexMap) =
              case Map.lookup (coords step) indexMap of
                Just index -> (step { idx = index }, indexMap)
                Nothing -> (step { idx = counter }, Map.insert (coords step) (idx step) indexMap)
        in newStep : loop steps (idx step + 1) newIndexMap
  in loop steps 0 Map.empty

findIntersections :: [[Move]] -> [Coords]
findIntersections = findPathIntersections . map (tracePath (Coords 0 0))

findPathIntersections :: [[Coords]] -> [Coords]
findPathIntersections =
  Set.toAscList .
  Set.delete (Coords 0 0) . foldr1 Set.intersection . map Set.fromList

manhattanDistance :: Coords -> Int
manhattanDistance coords = abs (x coords) + abs (y coords)

part1 :: [[Move]] -> Int
part1 = minimum . map manhattanDistance . findIntersections

part2 :: [[Move]] -> Int
part2 moves =
  let path = map (tracePath (Coords 0 0)) moves
      steps = map coordsToSteps path
      intersections = findPathIntersections path
      intersectionStepsAmount stepsList =
        map
          (\is -> idx $ fromJust $ find ((== is) . coords) stepsList)
          intersections
      stepAmounts1 = intersectionStepsAmount (head steps)
      stepAmounts2 = intersectionStepsAmount (last steps)
  in minimum (zipWith (+) stepAmounts1 stepAmounts2)

parseMoves :: String -> [Move]
parseMoves = map parseMove . wordsWhen (== ',')

main :: IO ()
main = do
  contents <- readFile "input/day3.txt"
  let wires = map parseMoves (lines contents)
  putStrLn ("Part 1: " ++ show (part1 wires))
  putStrLn ("Part 2: " ++ show (part2 wires))