module Solution where

data MinQueue a = Nil | Cons a (MinQueue a) Int deriving (Show)

empty :: Ord a => MinQueue a
empty = Nil

size :: Ord a => MinQueue a -> Int
size Nil = 0
size (Cons h t l) = l

useJust :: Ord a => Maybe a -> a
useJust (Just x) = x

findMin :: Ord a => a -> MinQueue a -> a
findMin x Nil = x
findMin x (Cons h t l) = if h < x then findMin h t else findMin x t

getMin :: Ord a => MinQueue a -> Maybe a
getMin Nil = Nothing
getMin (Cons h t l) = Just (findMin h t)

deleteMin :: Ord a => MinQueue a -> MinQueue a
deleteMin q = let x = getMin q in if x == Nothing then q else delete (useJust x) q

delete :: Ord a => a -> MinQueue a -> MinQueue a
delete x (Cons h Nil 1) = Nil
delete x (Cons h t l) = if h == x then t else Cons h (delete x t) (l - 1)

insert :: Ord a => a -> MinQueue a -> MinQueue a
insert x Nil = Cons x Nil 1
insert x (Cons h t l) = Cons x (Cons h t l) (l + 1)
