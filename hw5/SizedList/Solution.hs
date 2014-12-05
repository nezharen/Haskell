module Solution where

data SizedList a = Nil | Cons a (SizedList a) Int deriving (Show)

size :: SizedList a -> Int
size Nil = 0
size (Cons h t l) = l

head :: SizedList a -> a
head (Cons h t l) = h

tail :: SizedList a -> SizedList a
tail (Cons h t l) = t

init :: SizedList a -> SizedList a
init (Cons h Nil 1) = Nil
init (Cons h t l) = Cons h (Solution.init t) (l - 1)

last :: SizedList a -> a
last (Cons h Nil 1) = h
last (Cons h t l) = Solution.last t

