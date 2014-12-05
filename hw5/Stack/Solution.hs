module Solution where

data SizedList a = Nil | Cons a (SizedList a) Int deriving (Show)

mysize :: SizedList a -> Int
mysize Nil = 0
mysize (Cons h t l) = l

myhead :: SizedList a -> a
myhead (Cons h t l) = h

mytail :: SizedList a -> SizedList a
mytail (Cons h t l) = t

myinit :: SizedList a -> SizedList a
myinit (Cons h Nil 1) = Nil
myinit (Cons h t l) = Cons h (myinit t) (l - 1)

mylast :: SizedList a -> a
mylast (Cons h Nil 1) = h
mylast (Cons h t l) = mylast t

class Stack m where
	empty :: m a
	push :: a -> m a -> m a
	pop :: m a -> Maybe (m a)
	size :: m a -> Int

instance Stack SizedList where
	empty = Nil
	push x Nil = Cons x Nil 1
	push x (Cons h t l) = Cons h (push x t) (l + 1)
	pop Nil = Nothing
	pop (Cons x Nil 1) = Just Nil
	pop (Cons h t l) = Just (Cons h (useMaybe (pop t)) (l - 1))
	size = mysize

useMaybe :: Maybe (SizedList a) -> SizedList a
useMaybe maybe = case maybe of
	Nothing -> Nil
	Just x -> x

