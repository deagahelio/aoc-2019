module Day7 where

import Data.List (permutations, maximum)
import Day5 (Status(..), Computer(..), mkComputer, runProgram)
import Utils (wordsWhen)

inputPhaseSettings :: [Int] -> [Computer] -> [Computer]
inputPhaseSettings settings amps =
  map (\(i, amp) -> amp { _inputs = [settings !! i] }) (zip [0..] amps)

runAmplifiers :: (Int, [Computer]) -> (Int, [Computer])
runAmplifiers (signal, amps) =
  foldl
    (\(signal', amps') (i, amp) ->
       let runAmp = runProgram (amp { _inputs = _inputs amp ++ [signal'] })
       in (last (_outputs runAmp), amps' ++ [runAmp]))
    (signal, [])
    (zip [0..] amps)

part1 :: [Int] -> Int
part1 ints =
  let genAmps = (`inputPhaseSettings` replicate 5 (mkComputer ints))
  in maximum $ map (\settings -> fst $ runAmplifiers (0, genAmps settings)) (permutations [0..4])

runFeedbackLoop :: (Int, [Computer]) -> (Int, [Computer])
runFeedbackLoop (signal, amps) =
  let (signal', amps') = runAmplifiers (signal, amps)
  in if _status (head amps') == Halted
     then (signal', amps')
     else runFeedbackLoop (signal', amps')

part2 :: [Int] -> Int
part2 ints =
  let genAmps = (`inputPhaseSettings` replicate 5 (mkComputer ints))
  in maximum $ map (\settings -> fst $ runFeedbackLoop (0, genAmps settings)) (permutations [5..9])

main :: IO ()
main = do
  contents <- readFile "input/day7.txt"
  let ints = map read (wordsWhen (== ',') contents)
  putStrLn ("Part 1: " ++ show (part1 ints))
  putStrLn ("Part 2: " ++ show (part2 ints))