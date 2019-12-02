module Day2 where

import Utils (wordsWhen, replace)
import Data.Maybe (listToMaybe, fromJust)

data Status
  = Halt
  | Continue

data Computer =
  Computer
    { memory :: [Int]
    , pos :: Int
    }
  deriving Show
                        
mkComputer :: [Int] -> Computer
mkComputer memory' = Computer { memory = memory', pos = 0 }

runInstruction :: Computer -> (Status, Computer)
runInstruction computer =
  let memory' = memory computer
      pos' = pos computer
      opcode = memory' !! pos'
      operation f =
        let inValue1 = memory' !! (memory' !! (pos' + 1))
            inValue2 = memory' !! (memory' !! (pos' + 2))
            outPos = memory' !! (pos' + 3)
            newMemory = replace outPos (inValue1 `f` inValue2) memory'
        in computer { memory = newMemory, pos = pos' + 4 }
  in case opcode of
       1 -> (Continue, operation (+))
       2 -> (Continue, operation (*))
       99 -> (Halt, computer)
       _ -> error ("invalid opcode: " ++ show opcode)

runProgram :: Computer -> Computer
runProgram computer =
  let (status, newComputer) = runInstruction computer
  in case status of
       Halt -> newComputer
       Continue -> runProgram newComputer

runInputs :: Int -> Int -> Computer -> Int
runInputs noun verb computer =
  let memory' = replace 2 verb (replace 1 noun (memory computer))
      newComputer = runProgram (computer { memory = memory' })
  in head (memory newComputer)

part1 :: Computer -> Int
part1 = runInputs 12 2

findOutput :: Int -> Computer -> Maybe (Int, Int)
findOutput output computer =
  fmap fst $
  listToMaybe $
  dropWhile ((/= output) . snd) $
  concatMap
    (\noun ->
       map (\verb -> ((noun, verb), runInputs noun verb computer)) [0..99])
    [0..99]
  
part2 :: Computer -> Int
part2 computer = let (noun, verb) = fromJust (findOutput 19690720 computer)
                 in 100 * noun + verb

main :: IO ()
main = do
  contents <- readFile "input/day2.txt"
  let ints = map read (wordsWhen (== ',') contents)
  let computer = mkComputer ints
  putStrLn ("Part 1: " ++ show (part1 computer))
  putStrLn ("Part 2: " ++ show (part2 computer))