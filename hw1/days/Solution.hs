module Solution where

days :: Int -> Int -> Int
days year month = case month of
	1 -> 31
	2 -> if (year `mod` 400 == 0) || ((year `mod` 100 /= 0) && (year `mod` 4 == 0)) then 29 else 28
	3 -> 31
	4 -> 30
	5 -> 31
	6 -> 30
	7 -> 31
	8 -> 31
	9 -> 30
	10 -> 31
	11 -> 30
	12 -> 31
	_ -> 0

solution :: Int -> Int -> Int
solution = days
