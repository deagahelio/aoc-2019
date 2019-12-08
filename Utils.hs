module Utils where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

wordsEvery :: Int -> String -> [String]
wordsEvery _ [] = []
wordsEvery i s =
  let (w, s') = splitAt i s
  in w : wordsEvery i s'

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace i x' (x:xs)
  | i == 0 = x':xs
  | otherwise = x:replace (pred i) x' xs

tail' :: [a] -> [a]
tail' [] = []
tail' l = tail l

init' :: [a] -> [a]
init' [] = []
init' l = init l

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

pairs :: [a] -> [(a, a)]
pairs l = zip l (tail l)

digits :: Int -> [Int]
digits = map (read . return) . show