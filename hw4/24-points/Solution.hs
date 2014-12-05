module Solution where

check2 :: (Int, Int) -> Int -> Bool
check2 (x, y) t = if x + y == t || x - y == t || y - x == t || x * y == t || y /= 0 && x `mod` y == 0 && x `div` y == t || x /= 0 && y `mod` x == 0 && y `div` x == t then True else False

check3in2 :: (Int, Int) -> Int -> Int  -> Bool
check3in2 (x, y) z t = check2 (x, y) (t - z) || check2 (x, y) (t + z) || check2 (x, y) (z - t) || z /= 0 && t `mod` z == 0 && check2 (x, y) (t `div` z) || z /= 0 && check2 (x, y) (z * t) || z /= 0 && t /= 0 && z `mod` t == 0 && check2 (x, y) (z `div` t) || z == 0 && t == 0

check3 :: (Int, Int, Int) -> Int -> Bool
check3 (x, y, z) t = check3in2 (x, y) z t || check3in2 (x, z) y t || check3in2 (y, z) x t

check4in3 :: (Int, Int, Int) -> Int -> Int -> Bool
check4in3 (x, y, z) p t = check3 (x, y, z) (t - p) || check3 (x, y, z) (t + p) || check3 (x, y, z) (p - t) || p /= 0 && t `mod` p == 0 && check3 (x, y, z) (t `div` p) || p /= 0 && check3 (x, y, z) (p * t) || p /= 0 && t /= 0 && p `mod` t == 0 && check3 (x, y, z) (p `div` t) || p == 0 && t == 0

check4in2 :: (Int, Int) -> (Int, Int) -> Int -> Bool
check4in2 (x, y) (z, p) t = check3in2 (x, y) (z + p) t || check3in2 (x, y) (z - p) t || check3in2 (x, y) (p - z) t || check3in2 (x, y) (z * p) t || p /= 0 && z `mod` p == 0 && check3in2 (x, y) (z `div` p) t || z /= 0 && p `mod` z == 0 && check3in2 (x, y) (p `div` z) t

divide4to3 :: (Int, Int, Int, Int) -> Bool
divide4to3 (x, y, z, p) = check4in3 (x, y, z) p 24 || check4in3 (x, y, p) z 24 || check4in3 (x, z, p) y 24 || check4in3 (y, z, p) x 24

divide4to2 :: (Int, Int, Int, Int) -> Bool
divide4to2 (x, y, z, p) = check4in2 (x, y) (z, p) 24 || check4in2 (x, z) (y, p) 24 || check4in2 (x, p) (y, z) 24

solution :: (Int, Int, Int, Int) -> Bool
solution x = divide4to3 x || divide4to2 x
