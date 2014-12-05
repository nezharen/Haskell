module Solution where

mygcd :: Integral a => a -> a -> a
mygcd x 0 = x
mygcd x y = mygcd y (x `mod` y)

solution :: Integral a => a -> a -> a
solution = mygcd
