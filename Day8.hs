module Day8 where

import Utils (wordsEvery, count)
import Data.List (minimumBy)

data Image =
  Image
    { _width :: Int
    , _height :: Int
    , _layers :: [String]
    }

parseImage :: (Int, Int) -> String -> Image
parseImage (w, h) s =
  Image { _width = w
        , _height = h
        , _layers = wordsEvery (w * h) s
        }

part1 :: Image -> Int
part1 image =
  let zeroesCounts = map (count '0') (_layers image)
      layer =
        (_layers image !!) $
        fst $
        minimumBy (\a b -> compare (snd a) (snd b)) $
        zip [0..] zeroesCounts
  in count '1' layer * count '2' layer

getPixel :: Int -> Image -> Char
getPixel index image =
  let pixels = map (!! index) (_layers image)
      top = dropWhile (== '2') pixels
  in if null top
     then ' '
     else case head top of
            '0' -> '#'
            '1' -> '.'

part2 :: Image -> String
part2 image =
  let pixels = map (`getPixel` image) [0..(_width image * _height image) - 1]
  in unlines (wordsEvery (_width image) pixels)

main :: IO ()
main = do
  contents <- readFile "input/day8.txt"
  let image = parseImage (25, 6) contents
  putStrLn ("Part 1: " ++ show (part1 image))
  putStrLn "Part 2:"
  putStrLn (part2 image)