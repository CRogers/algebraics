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
cleanUp' :: Expr -> Expr
cleanUp' (Add (Num 0) b) = b
cleanUp' (Add a (Num 0)) = a
cleanUp' (Add (Sym x) (Sym y)) | x == y = Mul (Num 2) (Sym x)
--cleanUp' (Add a (Add b c)) = associateLeft Add a b c
--cleanUp' (Add (Add a b) c) = associateRight Add a b c

cleanUp' (Mul (Num 0) _) = Num 0
cleanUp' (Mul _ (Num 0)) = Num 0
cleanUp' (Mul (Num 1) b) = b
cleanUp' (Mul a (Num 1)) = a
cleanUp' (Mul (Num a) (Sub (Num 0) b)) = Mul (Num (0 - a)) b
--cleanUp' (Mul a (Mul b c)) = associateLeft Mul a b c
--cleanUp' (Mul (Mul a b) c) = associateRight Mul a b c

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