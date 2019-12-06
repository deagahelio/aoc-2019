module Day6 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.Maybe (maybeToList)
import Data.List ((\\), minimum)

orbitCount :: String -> Map String String -> Int
orbitCount object orbits =
  case Map.lookup object orbits of
    Just object' -> orbitCount object' orbits + 1
    Nothing -> 0
    
part1 :: Map String String -> Int
part1 orbits = sum $ map (`orbitCount` orbits) (Map.keys orbits)

findOrbiting :: String -> Map String String -> [String]
findOrbiting object = Map.keys . Map.filter (== object)

connectedObjects :: String -> Map String String -> [String]
connectedObjects object orbits =
  findOrbiting object orbits ++ maybeToList (Map.lookup object orbits)

findPaths :: String -> String -> [String] -> Map String String -> [[String]]
findPaths start end visited orbits =
  if start == end
  then [[start]]
  else
    let objects = connectedObjects start orbits \\ visited
    in concatMap (\start' -> map (start :) $ findPaths start' end (start : visited) orbits) objects

part2 :: Map String String -> Int
part2 = subtract 3 . minimum . map length . findPaths "YOU" "SAN" []

main :: IO ()
main = do
  contents <- readFile "input/day6.txt"
  let orbits = Map.fromList $ map (swap . fmap tail . splitAt 3) (lines contents)
  putStrLn ("Part 1: " ++ show (part1 orbits))
  putStrLn ("Part 2: " ++ show (part2 orbits))