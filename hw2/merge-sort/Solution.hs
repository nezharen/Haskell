module Solution where

merge :: Integral a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) = if x <= y then x:merge xs (y:ys) else y:merge (x:xs) ys

solution :: Integral a => [a] -> [a] -> [a]
solution = merge
