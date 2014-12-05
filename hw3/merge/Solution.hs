module Solution where

merge :: [Integer] -> [Integer] -> [Integer]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) = if x <= y then x:merge xs (y:ys) else y:merge (x:xs) ys

mergesort :: [Integer] -> [Integer]
mergesort [] = []
mergesort (x:[]) = x:[]
mergesort x = merge (mergesort (take t x)) (mergesort (drop t x))
              where l = length x
                    t = l `div` 2

solution :: [Integer] -> [Integer]
solution = mergesort
