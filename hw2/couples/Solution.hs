module Solution where

notfound :: Integer -> [Integer] -> Integer
notfound x [] = 1
notfound x (y:ys) = if x == y then 0 else notfound x ys

check :: [Integer] -> [Integer] -> Integer
check [] y = 0
check (x:xs) y = notfound x y + check xs y

juage :: [Integer] -> [Integer] -> Integer
juage x y = check x y + check y x

solution :: [Integer] -> [Integer] -> Integer
solution = juage
