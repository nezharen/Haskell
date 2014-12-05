module Solution where

f :: Integer -> [Integer]
f 0 = [0]
f 1 = [1]
f 2 = [1, 1, 0]
f x = init (sum t : t)
      where t = f (x - 1)

solution :: Integer -> Integer
solution x = head (f x)
