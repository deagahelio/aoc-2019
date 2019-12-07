module Day5 where

import Utils (wordsWhen, replace, digits, init')

data Status
  = Halted
  | Running
  | Waiting
  deriving (Show, Eq)

data Computer =
  Computer
    { _memory :: [Int]
    , _pos :: Int
    , _status :: Status
    , _inputs :: [Int]
    , _outputs :: [Int]
    }
  deriving Show

mkComputer :: [Int] -> Computer
mkComputer memory =
  Computer { _memory = memory
           , _pos = 0
           , _status = Running
           , _inputs = []
           , _outputs = []
           }

data ParameterMode
  = Position
  | Immediate
  deriving Show

getParameterMode :: Int -> ParameterMode
getParameterMode digit =
  case digit of
    0 -> Position
    1 -> Immediate
    _ -> error ("invalid parameter mode: " ++ show digit)

getParameterModes :: Int -> [ParameterMode]
getParameterModes opcode =
  let modes = reverse $ map getParameterMode $ init' $ init' $ digits opcode
      padding = replicate (3 - length modes) Position
  in modes ++ padding

getOpcode :: Int -> Int
getOpcode = (`mod` 100)

(!!!) :: [Int] -> (Int, ParameterMode) -> Int
l !!! (i, pm) =
  case pm of
    Position -> l !! (l !! i)
    Immediate -> l !!i

type Operation = Computer -> Computer

operationBin :: (Int -> Int -> Int) -> Operation
operationBin op computer =
  let memory = _memory computer
      pos = _pos computer
      [inMode1, inMode2, _] = getParameterModes (memory !! pos)
      in1 = memory !!! (pos + 1, inMode1)
      in2 = memory !!! (pos + 2, inMode2)
      out1 = memory !! (pos + 3)
      memory' = replace out1 (op in1 in2) memory 
  in computer { _memory = memory', _pos = pos + 4 }

operationAdd :: Operation
operationAdd = operationBin (+)

operationMul :: Operation
operationMul = operationBin (*)

operationInput :: Operation
operationInput computer =
  let memory = _memory computer
      pos = _pos computer
      out1 = memory !! (pos + 1)
      (input:inputs') = _inputs computer
      memory' = replace out1 input memory
  in
    if null (_inputs computer)
    then computer { _status = Waiting }
    else computer { _memory = memory', _pos = pos + 2, _inputs = inputs' }

operationOutput :: Operation
operationOutput computer =
  let memory = _memory computer
      pos = _pos computer
      inMode1 = head $ getParameterModes (memory !! pos)
      in1 = memory !!! (pos + 1, inMode1)
  in computer { _pos = pos + 2, _outputs = _outputs computer ++ [in1] }

operationJumpIf :: (Int -> Bool) -> Operation
operationJumpIf op computer =
  let memory = _memory computer
      pos = _pos computer
      [inMode1, inMode2, _] = getParameterModes (memory !! pos)
      in1 = memory !!! (pos + 1, inMode1)
      in2 = memory !!! (pos + 2, inMode2)
      pos' =
        if op in1
        then in2
        else pos + 3
  in computer { _pos = pos' }

operationJumpIfTrue :: Operation
operationJumpIfTrue = operationJumpIf (/= 0)

operationJumpIfFalse :: Operation
operationJumpIfFalse = operationJumpIf (== 0)

operationCompare :: (Int -> Int -> Bool) -> Operation
operationCompare op computer =
  let memory = _memory computer
      pos = _pos computer
      [inMode1, inMode2, _] = getParameterModes (memory !! pos)
      in1 = memory !!! (pos + 1, inMode1)
      in2 = memory !!! (pos + 2, inMode2)
      out1 = memory !! (pos + 3)
      memory' = replace out1 (fromEnum $ op in1 in2) memory
  in computer { _memory = memory', _pos = pos + 4 }

operationLessThan :: Operation
operationLessThan = operationCompare (<)

operationEquals :: Operation
operationEquals = operationCompare (==)

operationHalt :: Operation
operationHalt computer = computer { _status = Halted }

getNext :: Computer -> Operation
getNext computer =
  let opcode = getOpcode (_memory computer !! _pos computer)
  in case opcode of
       1 -> operationAdd
       2 -> operationMul
       3 -> operationInput
       4 -> operationOutput
       5 -> operationJumpIfTrue
       6 -> operationJumpIfFalse
       7 -> operationLessThan
       8 -> operationEquals
       99 -> operationHalt
       _ -> error ("invalid opcode: " ++ show opcode)

runNext :: Operation
runNext computer = getNext computer computer

runProgram :: Operation
runProgram computer =
  let computer' = runNext computer
  in case _status computer' of
       Halted -> computer'
       Running -> runProgram computer'
       Waiting -> computer' { _status = Running }

runWithInput :: Int -> [Int] -> [Int]
runWithInput id ints =
  _outputs $ runProgram $ (mkComputer ints) { _inputs = [id] }

part1 :: [Int] -> [Int]
part1 = runWithInput 1

part2 :: [Int] -> [Int]
part2 = runWithInput 5

main :: IO ()
main = do
  contents <- readFile "input/day5.txt"
  let ints = map read (wordsWhen (== ',') contents)
  putStrLn ("Part 1: " ++ show (part1 ints))
  putStrLn ("Part 2: " ++ show (part2 ints))