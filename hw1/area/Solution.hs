module Solution where

area :: (Integer, Integer) -> (Integer, Integer) -> Rational
area (x1, y1) (x2, y2) = toRational (abs (x1 * y2 - x2 * y1)) / 2

solution :: (Integer, Integer) -> (Integer, Integer) -> Rational
solution = area
