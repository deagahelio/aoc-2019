module Utils where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'
                        
replace :: Int -> a -> [a] -> [a]
replace i x l = let (ll, _:lr) = splitAt i l
                in ll ++ x:lr

tail' :: [a] -> [a]
tail' [] = []
tail' l = tail l

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

pairs :: [a] -> [(a, a)]
pairs l = zip (init l) (tail l)