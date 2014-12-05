module Solution where

data MyQueue a = Nil | Cons a (MyQueue a) Int deriving (Show)

mysize :: MyQueue a -> Int
mysize Nil = 0
mysize (Cons h t l) = l

myhead :: MyQueue a -> a
myhead (Cons h t l) = h

mytail :: MyQueue a -> MyQueue a
mytail (Cons h t l) = t

myinit :: MyQueue a -> MyQueue a
myinit (Cons h Nil 1) = Nil
myinit (Cons h t l) = Cons h (myinit t) (l - 1)

mylast :: MyQueue a -> a
mylast (Cons h Nil 1) = h
mylast (Cons h t l) = mylast t

class Queue m where
	empty :: m a
	enqueue :: a -> m a -> m a
	dequeue :: m a -> Maybe (m a)
	size :: m a -> Int

instance Queue MyQueue where
	empty = Nil
	enqueue x Nil = Cons x Nil 1
	enqueue x (Cons h t l) = Cons h (enqueue x t) (l + 1)
	dequeue Nil = Nothing
	dequeue (Cons x Nil 1) = Just Nil
	dequeue (Cons h t l) = Just (Cons (myhead t) (mytail t) (l - 1))
	size = mysize

