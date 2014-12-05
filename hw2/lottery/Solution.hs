module Solution where

test :: (Int, Int, Int) -> (Int, Int, Int) -> Int
test x y = if x == y then 1 else 0

count :: (Int, Int, Int) -> (Int, Int, Int) -> Int
count (x1, y1, z1) (x2, y2, z2) = test (x1, y1, z1) (x2, y2, z2) + test (x1, y1, z1) (x2, z2, y2) + test (x1, y1, z1) (y2, x2, z2) + test (x1, y1, z1) (y2, z2, x2) + test (x1, y1, z1) (z2, x2, y2) + test (x1, y1, z1) (z2, y2, x2)   

juage :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Int
juage 1 x y = if x == y then 1040 else 0
juage 2 (x1, y1, z1) (x2, y2, z2) = case count (x1, y1, z1) (x2, y2, z2) of
	0 -> 0
	1 -> 173
	_ -> 346

solution :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Int
solution = juage
