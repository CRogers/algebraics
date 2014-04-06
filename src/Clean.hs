{-# LANGUAGE PatternSynonyms #-}

module Clean where

import Fix(bottomUp)
import Tree

eval :: Expr -> Expr
eval (Add (Num a) (Num b)) = Num $ a + b
eval (Sub (Num a) (Num b)) = Num $ a - b
eval (Mul (Num a) (Num b)) = Num $ a * b
eval (Div (Num a) (Num b)) = Num $ a / b
eval (Pow (Num a) (Num b)) = Num $ a ** b
eval e = e
{-
associateLeft :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr -> Expr
associateLeft tf a b c = if cleanedUp == assoc then tf a (tf b c) else cleanedUp
	where assoc = tf (tf a b) c
	      cleanedUp = cleanUp' assoc

associateRight :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr -> Expr
associateRight tf a b c = if cleanedUp == assoc then tf (tf a b) c else cleanedUp
	where assoc = tf a (tf b c)
	      cleanedUp = cleanUp' assoc
-}

pattern Neg a = Sub (Num 0) a

cleanUp' :: Expr -> Expr
cleanUp' (Add (Num 0) b) = b
cleanUp' (Add a (Num 0)) = a
cleanUp' (Add a b) | a == b = Mul (Num 2) a
--cleanUp' (Add a (Sub (Num 0) b)) = Add (Sub (Num 0) a) b
--cleanUp' (Add a (Add b c)) = associateLeft Add a b c
--cleanUp' (Add (Add a b) c) = associateRight Add a b c

cleanUp' (Sub a (Num 0)) = a
cleanUp' (Sub a b) | a == b = Num 0
cleanUp' (Sub a (Neg b)) = Add a b
cleanUp' (Add (Neg a) b) = Sub b a

cleanUp' (Mul (Num 0) _) = Num 0
cleanUp' (Mul _ (Num 0)) = Num 0
cleanUp' (Mul (Num 1) b) = b
cleanUp' (Mul a (Num 1)) = a
cleanUp' (Mul a (Neg b)) = Neg (Mul a b)
cleanUp' (Mul (Neg a) b) = Neg (Mul a b)
--cleanUp' (Mul a (Mul b c)) = associateLeft Mul a b c
--cleanUp' (Mul (Mul a b) c) = associateRight Mul a b c

cleanUp' (Div a (Num 1)) = a
cleanUp' (Div a b) | a == b = Num 1
cleanUp' (Mul (Div (Num 1) a) b) | a == b = Num 1
cleanUp' (Mul a (Div (Num 1) b)) | a == b = Num 1

cleanUp' (Pow _ (Num 0)) = Num 1
cleanUp' (Pow a (Num 1)) = a
cleanUp' (Pow (Num 0) _) = Num 0
cleanUp' (Pow (Num 1) _) = Num 1
cleanUp' (Pow (Pow a b) c) = Pow a (Mul b c) 

cleanUp' e = eval e

limit :: Eq a => (a -> a) -> a -> a
limit f prev = if next == prev then prev else limit f next 
	where next = f prev

cleanUp :: Expr -> Expr
cleanUp = limit (bottomUp cleanUp')