module Solution where

area :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Rational
area (x1, y1) (x2, y2) (x3, y3) = toRational (abs $ (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)) / 2

solution :: [(Int, Int)] -> Rational
solution [] = 0
solution (x:[]) = 0
solution (x:y:[]) = 0
solution (x:y:z:ls) = area x y z + solution (x:z:ls)
