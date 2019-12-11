module Day10 where

import Linear.V2
import Data.List (delete, (\\), maximumBy, sortOn, elemIndex)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseAsteroids :: String -> [V2 Int]
parseAsteroids =
  map snd . filter ((== '#') . fst) . concatMap (\(y, row) -> zip row $ map (`V2` y) [0..]) . zip [0..] . lines

reduce :: V2 Int -> V2 Int
reduce (V2 0 y) = V2 0 (signum y)
reduce (V2 x 0) = V2 (signum x) 0
reduce v@(V2 x y) = fmap (`quot` gcd x y) v

los :: V2 Int -> V2 Int -> [V2 Int]
los from to =
  takeWhile ((`elem` [compare from to, EQ]) . (`compare` to)) $ iterate (reduce (to - from) +) from

directLos :: [V2 Int] -> [V2 Int] -> [V2 Int]
directLos los blocks =
  takeWhile (not . (`elem` blocks)) los

visibleAsteroids :: V2 Int -> [V2 Int] -> [V2 Int]
visibleAsteroids from asteroids =
  let isVisible to = to `elem` directLos (los from to) (asteroids \\ [from, to])
  in filter isVisible (delete from asteroids)

part1 :: [V2 Int] -> (V2 Int, Int)
part1 asteroids =
  maximumBy (comparing snd) $ zip asteroids $ map (length . (`visibleAsteroids` asteroids)) asteroids

sortAsteroids :: V2 Int -> [V2 Int] -> [V2 Int]
sortAsteroids from asteroids =
  let angle to =
        let (V2 x y) = fmap fromIntegral (to - from)
        in atan2 x y
      angleMap =
        foldr (\as mp -> Map.insert as (angle as) mp) Map.empty asteroids
  in map fst $ sortOn (negate . snd) (Map.toAscList angleMap)

vaporizationOrder :: V2 Int -> [V2 Int] -> [V2 Int]
vaporizationOrder _ [] = []
vaporizationOrder from asteroids =
  let visibles = sortAsteroids from $ visibleAsteroids from asteroids
  in visibles ++ vaporizationOrder from (asteroids \\ visibles)

part2 :: V2 Int -> [V2 Int] -> Int
part2 from asteroids =
  let (V2 x y) = vaporizationOrder from asteroids !! 199
  in x * 100 + y

main :: IO ()
main = do
  contents <- readFile "input/day10.txt"
  let asteroids = parseAsteroids contents
  let (station, detectables) = part1 asteroids
  putStrLn ("Part 1: " ++ show detectables)
  putStrLn ("Part 2: " ++ show (part2 station asteroids))