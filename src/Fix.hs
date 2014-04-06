{-# LANGUAGE UndecidableInstances #-}

module Fix where

import Control.Arrow ((>>>))

newtype Fix f = In { out :: (f (Fix f)) }

instance Show (f (Fix f)) => Show (Fix f) where
	show (In f) = "(" ++ show f ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
	(In x) == (In y) = x == y

instance Ord (f (Fix f)) => Ord (Fix f) where
	(In x) `compare` (In y) = x `compare` y

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = g
	where g = f . fmap g . out

bottomUp' :: Functor f => (Fix f -> Fix f -> Fix f) -> Fix f -> Fix f
bottomUp' f x = bottomUpI x x
	where bottomUpI y = out >>> fmap (bottomUp' f) >>> In >>> f y

bottomUp :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f