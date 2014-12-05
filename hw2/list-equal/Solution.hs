module Solution where

equal :: Eq a => [a] -> [a] -> Bool
equal x y = not (x /= y)

solution :: Eq a => [a] -> [a] -> Bool
solution = equal
