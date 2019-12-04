module Day4 where

import Utils (wordsWhen, count, pairs)

type Predicate = Int -> Bool

twoAdjacents :: Predicate
twoAdjacents = any (uncurry (==)) . pairs . show

twoAdjacents' :: Predicate
twoAdjacents' n = any (\(cur, next) -> cur == next && count cur (show n) == 2) $ pairs $ show n

increasing :: Predicate
increasing = all (uncurry (<=)) . pairs . show

checkPredicates :: [Predicate] -> Int -> Bool
checkPredicates preds n = all ($ n) preds

validPasswords :: [Predicate] -> [Int] -> Int
validPasswords preds = count True . map (checkPredicates preds)

part1 :: [Int] -> Int
part1 = validPasswords [twoAdjacents, increasing]

part2 :: [Int] -> Int
part2 = validPasswords [twoAdjacents', increasing]

main :: IO ()
main = do
  contents <- readFile "input/day4.txt"
  let [lower, upper] = map read (wordsWhen (== '-') contents)
  putStrLn ("Part 1: " ++ show (part1 [lower..upper]))
  putStrLn ("Part 2: " ++ show (part2 [lower..upper]))