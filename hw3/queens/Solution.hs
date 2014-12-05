module Solution where

pos :: Int -> (Char, Int)
pos x = ("abcdefgh" !! (x `div` 8), x `mod` 8 + 1)

find :: [Char] -> Char -> Int
find [] y = 0
find (x:xs) y = if x == y then 0 else 1 + find xs y

hang :: [(Char, Int)] -> Int -> Bool
hang [] y = True
hang ((x1,x2):ls) y = if x2 == y `mod` 8 + 1 then False else hang ls y

lie :: [(Char, Int)] -> Int -> Bool
lie [] y = True
lie ((x1,x2):ls) y = if x1 == "abcdefgh" !! (y `div` 8) then False else lie ls y

xie :: [(Char, Int)] -> Int -> Bool
xie [] y = True
xie ((x1,x2):ls) y = if y `div` 8 - t == y `mod` 8 + 1 - x2 || y `div` 8 - t == x2 - y `mod` 8 - 1 then False else xie ls y
                     where t = find "abcdefgh" x1 

check :: [(Char, Int)] -> Int -> Bool
check x y =  hang x y && lie x y && xie x y

test :: [(Char, Int)] -> Int -> Int
test x 64 = 0
test x y = if check x y then max (1 + test (pos y:x) (y + 1)) (test x (y + 1)) else test x (y + 1)

solution :: [(Char, Int)] -> Int
solution [] = 8
solution x = test x 0
