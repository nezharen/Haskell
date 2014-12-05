module Solution where

find :: Integer -> Char
find x = case x `mod` 9 of
	0 -> 'a'
	1 -> 'b'
	2 -> 'c'
	3 -> 'd'
	4 -> 'e'
	5 -> 'f'
	6 -> 'g'
	7 -> 'h'
	8 -> 'i'

count :: Integer -> Int -> [Char]
count x 0 = []
count x y = (find x):(count (x `div` 9) (y - 1))

juage :: Integer -> [Char]
juage x = reverse $ count (x - 1) 9

solution :: Integer -> [Char]
solution = juage
