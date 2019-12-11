module Day11 where

import Linear.V2
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Left, Right)
import Day9 (Status(..), Computer(..), mkComputer, runProgram)
import Utils (wordsWhen)
import Debug.Trace

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Enum)

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft dir = pred dir

turnRight :: Direction -> Direction
turnRight Left = Up
turnRight dir = succ dir

offset :: Direction -> V2 Int
offset Up = V2 0 (-1)
offset Right = V2 1 0
offset Down = V2 0 1
offset Left = V2 (-1) 0

data Robot =
  Robot
    { _computer :: Computer
    , _location :: V2 Int
    , _direction :: Direction
    , _whites :: Set (V2 Int)
    , _painted :: Set (V2 Int)
    }
  deriving Show

mkRobot :: [Int] -> Robot
mkRobot ints =
  Robot
    { _computer = mkComputer ints
    , _location = V2 0 0
    , _direction = Up
    , _whites = Set.empty
    , _painted = Set.empty
    }

runRobot :: Robot -> Robot
runRobot robot =
  let computer' = runProgram (_computer robot)
      (whites', direction') =
        case _outputs computer' of
          [panelColor, turn] ->
            let operation =
                  case panelColor of
                    0 -> Set.delete 
                    1 -> Set.insert
                operation' =
                  case turn of
                    0 -> turnLeft
                    1 -> turnRight
            in (operation (_location robot) (_whites robot),
                operation' (_direction robot))
          [] -> (_whites robot, _direction robot)
      painted' = Set.insert (_location robot) (_painted robot)
      location' = _location robot + offset direction'
      robot' = robot { _computer = computer' { _outputs = [] }
                     , _location = location'
                     , _direction = direction'
                     , _whites = whites'
                     , _painted = painted'
                     }
  in case _status computer' of
       Running ->
         let panel = fromEnum $ Set.member (_location robot') (_whites robot')
         in runRobot (robot' { _computer = (_computer robot') { _inputs = [panel] } })
       Halted -> robot'

part1 :: [Int] -> Int
part1 = Set.size . _painted . runRobot . mkRobot

part2 :: [Int] -> String
part2 ints =
  let robot = runRobot $ (mkRobot ints) { _whites = Set.singleton (V2 0 (-1)) }
      zipWithV2 f (V2 ax ay) (V2 bx by) = V2 (f ax bx) (f ay by)
      (V2 minX minY) = Set.foldr (zipWithV2 min) (V2 0 0) (_whites robot)
      (V2 maxX maxY) = Set.foldr (zipWithV2 max) (V2 0 0) (_whites robot)
      printPoint point =
        if Set.member point (_whites robot)
        then '#'
        else '.'
  in intercalate "\n" $ map (\y -> map (\x -> printPoint (V2 x y)) [minX..maxX]) [minY..maxY]

main :: IO ()
main = do
  contents <- readFile "input/day11.txt"
  let ints = map read (wordsWhen (== ',') contents)
  putStrLn ("Part 1: " ++ show (part1 ints))
  putStrLn "Part 2:"
  putStrLn (part2 ints)