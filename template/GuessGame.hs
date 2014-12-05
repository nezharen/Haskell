module GuessGame where

import System.Random (randomR, StdGen)

type Step = ([Int], Int, Int)
type History = [Step]

type SingleRate = (Int, Rational)
type Rates = [SingleRate]

multiple :: [Rational] -> Int -> Rational -> [Rational]
multiple (r:rs) 0 y = (r * y) : rs
multiple (r:rs) x y = r : multiple rs (x - 1) y

redeal :: Step -> [Rational] -> Int -> [Rational]
redeal (result, a, b) rate 10 = rate
redeal (result, a, b) rate x =
	let t = if (x `elem` result) then rate else multiple rate x (toRational (4 - a) / 6)
	in redeal (result, a, b) t (x + 1)

deal :: Step -> Int -> Int -> [Rational] -> [Rational]
deal (result, a, b) x 4 rate = redeal (result, a, b) rate 0
deal (result, a, b) x y rate = 
	let t = if x == y then multiple rate (result !! y) (toRational b / 4) else multiple rate (result !! y) (toRational (a - b) / 4)
	in deal (result, a, b) x (y + 1) t

count :: History -> Int -> [Rational] -> [Rational]
count [] x rate = rate
count (h:hs) x rate = count hs x (deal h x 0 rate)

get :: History -> Int -> [Rational]
get history 0  = count history 0 ((0 :: Rational) : replicate 9 (1 :: Rational))
get history x = count history x (replicate 10 (1 :: Rational))

convert :: [Rational] -> Rates -> Int -> Rates
convert [] rates x = rates
convert (r:rs) rates x = if r == 0 then convert rs rates (x + 1) else convert rs ((x, r) : rates) (x + 1)

compose :: SingleRate -> SingleRate -> SingleRate -> SingleRate -> ([Int], Rational)
compose (t0, r0) (t1, r1) (t2, r2) (t3, r3) = ([t0, t1, t2, t3], r0 * r1 * r2 * r3)

checkAnswer :: [Int] -> [Int] -> (Int, Int)
checkAnswer ans try = (appeared, inposition) where
	appeared = length (filter (`elem` ans) try)
	inposition = let bs = (zipWith (==) ans try) in length $ filter id bs

check :: History -> [Int] -> Bool
check [] result = True
check ((try, a, b):hs) result = (checkAnswer result try == (a, b)) && check hs result

juage :: History -> (Rates, Rates, Rates, Rates) -> (Int, Int, Int, Int) -> ([Int], Rational) -> [Int]
juage history (rates0, rates1, rates2, rates3) (a, b, c, d) (result, rate) =
	if a == length rates0 then result else
		if b == length rates1 then juage history (rates0, rates1, rates2, rates3) (a + 1, 0, 0, 0) (result, rate) else
			if c == length rates2 then juage history (rates0, rates1, rates2, rates3) (a, b + 1, 0, 0) (result, rate) else
				if d == length rates3 then juage history (rates0, rates1, rates2, rates3) (a, b, c + 1, 0) (result, rate) else
					let ((t0, r0), (t1, r1), (t2, r2), (t3, r3)) = (rates0 !! a, rates1 !! b, rates2 !! c, rates3 !!d) in
						if t0 == t1 then juage history (rates0, rates1, rates2, rates3) (a, b + 1, 0, 0) (result, rate) else
							if t0 == t2 || t1 == t2 then juage history (rates0, rates1, rates2, rates3) (a, b, c + 1, 0) (result, rate) else
								if t0 == t3 || t1 == t3 || t2 == t3 then juage history (rates0, rates1, rates2, rates3) (a, b, c, d + 1) (result, rate) else
									let (tresult, trate) = compose (t0, r0) (t1, r1) (t2, r2) (t3, r3) in
										if trate > rate && check history tresult then juage history (rates0, rates1, rates2, rates3) (a, b, c, d + 1) (tresult, trate)
										else juage history (rates0, rates1, rates2, rates3) (a, b, c, d + 1) (result, rate)

work :: History -> [Rates] -> Int -> [Int]
work history rates 4 = juage history ((rates !! 3), (rates !! 2), (rates !! 1), (rates !! 0)) (0, 0, 0, 0) ([0, 0, 0, 0], 0)
work history rates x = let t = get history x in work history (convert t [] 0 : rates) (x + 1)

guessMove :: History -> StdGen -> ([Int], StdGen)
guessMove [] ranGen = ([9, 8, 7, 6], ranGen)
guessMove [x] ranGen = ([6, 5, 4, 3], ranGen)
guessMove [x, y] ranGen = ([3, 2, 1, 0], ranGen)
guessMove [x, y, z] ranGen = ([8, 7, 2, 1], ranGen)
guessMove history ranGen = (work history [] 0, ranGen)

