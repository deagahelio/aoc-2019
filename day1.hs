module AOC.Day1 where

fuelRequirement :: Int -> Int
fuelRequirement = subtract 2 . (`quot` 3)

part1 :: [Int] -> Int
part1 = sum . map fuelRequirement

fuelFuelRequirement :: Int -> Int
fuelFuelRequirement = sum . takeWhile (> 0) . tail . iterate fuelRequirement

part2 :: [Int] -> Int
part2 = sum . map fuelFuelRequirement

main :: IO ()
main = do
  contents <- readFile "input/day1.txt"
  let masses = map read (lines contents)
  putStrLn ("Part 1: " ++ show (part1 masses))
  putStrLn ("Part 2: " ++ show (part2 masses))