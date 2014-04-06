{-# LANGUAGE DeriveFunctor #-}

module RawTree where

data Expr a
	= Sym String
	| Num Double
	| Add a a
	| Sub a a
	| Div a a
	| Mul a a
	| Pow a a
	| Sin a
	| Cos a
	deriving (Show, Eq, Functor)

pretty2 :: String -> String -> String -> String
pretty2 sep a b = "(" ++ a ++ " " ++ sep ++ " " ++ b ++ ")"

pretty :: Expr String -> String
pretty (Sym a) = a
pretty (Num n) = show n
pretty (Add a b) = pretty2 "+" a b
pretty (Sub a b) = pretty2 "-" a b
pretty (Mul a b) = pretty2 "*" a b
pretty (Div a b) = pretty2 "/" a b
pretty (Pow a b) = pretty2 "^" a b
pretty (Sin x) = "sin " ++ x
pretty (Cos x) = "cos " ++ x 