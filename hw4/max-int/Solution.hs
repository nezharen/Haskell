module Solution where

make :: Int -> [Char]
make 0 = []
make x = make (x `div` 10) ++ ["0123456789" !! (x `mod` 10)]

count :: Int -> [Char]
count 1 = "1"
count x = make x ++ count (x - 1)

remove :: [Char] -> [Char] -> Int -> [Char]
remove [] y 0 = y
remove (x:xs) y 0 = remove xs (x:y) 0
remove [] (y:[]) m = remove [] [] (m - 1)
remove (x:xs) (y:[]) m = remove xs (x:[]) (m - 1)
remove [] (y1:y2:ys) m = if y1 < y2 then remove [] (y2:ys) (m - 1) else remove [y1] (y2:ys) m
remove (x:xs) (y1:y2:ys) m = if y1 < y2 then remove xs (x:y2:ys) (m - 1) else remove (y1:x:xs) (y2:ys) m

solution :: Int -> Int -> [Char]
solution x m = remove [] (count x) m
