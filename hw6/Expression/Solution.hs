module Solution where
import Data.Char

useJust :: Maybe Integer -> Integer
useJust (Just x) = x

count1 :: ([Maybe Integer], [Char]) -> ([Maybe Integer], [Char])
count1 ([i], []) = ([i], [])
count1 (is, '(':ops) = (is, '(':ops)
count1 (x:y:is, op:ops) = if x == Nothing || y == Nothing then count1 (Nothing:is, ops) else case op of
	'+' -> count1 (Just (useJust y + useJust x):is, ops)
	'-' -> count1 (Just (useJust y - useJust x):is, ops)
	'*' -> count1 (Just (useJust y * useJust x):is, ops)
	'/' -> if x == Just 0 then count1 (Nothing:is, ops) else count1 (Just (useJust y `div` useJust x):is, ops)

count2 :: ([Maybe Integer], [Char]) -> ([Maybe Integer], [Char])
count2 ([i], []) = ([i], [])
count2 (is, '(':ops) = (is, '(':ops)
count2 (x:y:is, op:ops) = case op of
	'+' -> (x:y:is, op:ops)
	'-' -> (x:y:is, op:ops)
	'*' -> if x == Nothing || y == Nothing then count2 (Nothing:is, ops) else count2 (Just (useJust y * useJust x):is, ops)
	'/' -> if x == Nothing || y == Nothing || x == Just 0 then count2 (Nothing:is, ops) else count2 (Just (useJust y `div` useJust x):is, ops)

count3 :: ([Maybe Integer], [Char]) -> ([Maybe Integer], [Char])
count3 (is, '(':ops) = (is, ops)
count3 (x:y:is, op:ops) = if x == Nothing || y == Nothing then count3 (Nothing:is, ops) else case op of
	'+' -> count3 (Just (useJust y + useJust x):is, ops)
	'-' -> count3 (Just (useJust y - useJust x):is, ops)
	'*' -> count3 (Just (useJust y * useJust x):is, ops)
	'/' -> if x == Just 0 then count3 (Nothing:is, ops) else count3 (Just (useJust y `div` useJust x):is, ops)

work :: ([Char], ([Maybe Integer], [Char]), Maybe Integer) -> Maybe Integer
work ([], ([x], []), _) = x
work ([], (is, ops), i) = work ([], count1 (is, ops), i)
work ([c], (is, ops), i) = case c of
	')' -> work ([], count3 (i:is, ops), Just 0)
	p -> if i == Nothing then Nothing else work ([], (Just (useJust i * 10 + toInteger (ord p - ord '0')):is, ops), Just 0)
work (c:cs, (is, ops), i) = case c of
	'+' -> let (tis, tops) = count1 (i:is, ops) in work (cs, (tis, '+':tops), Just 0)
	'-' -> let (tis, tops) = count1 (i:is, ops) in work (cs, (tis, '-':tops), Just 0)
	'*' -> let (tis, tops) = count2 (i:is, ops) in work (cs, (tis, '*':tops), Just 0)
	'/' -> let (tis, tops) = count2 (i:is, ops) in work (cs, (tis, '/':tops), Just 0)
	'(' -> work (cs, (is, '(':ops), i)
	')' -> let (ti:tis, tops) = count3 (i:is, ops) in work (cs, (tis, tops), ti)
	p -> if i == Nothing then Nothing else work (cs, (is, ops), Just (useJust i * 10 + toInteger (ord p - ord '0')))

solution :: [Char] -> Maybe Integer
solution x = work (x, ([], []), Just 0)
