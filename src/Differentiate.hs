module Differentiate where

import Fix(bottomUp')
import Tree
import Clean(cleanUp)

differentiate' :: Expr -> Expr -> Expr
differentiate' (Num _) _ = Num 0
differentiate' (Sym _) _ = Num 1
differentiate' (Mul a b) (Mul a' b') = Add (Mul a b') (Mul a' b)
differentiate' (Div a b) (Div a' b') = Div top bottom
	where top = Sub (Mul a' b) (Mul a b')
	      bottom = Pow b (Num 2)
differentiate' (Pow a b) (Pow a' _) = Mul a' (Mul b (Pow a (Sub b (Num 1))))
differentiate' (Cos x) (Cos x') = Mul x' (Sub (Num 0) (Sin x))
differentiate' (Sin x) (Sin x') = Mul x' (Cos x)
differentiate' _ e = e

differentiate :: Expr -> Expr
differentiate = cleanUp . bottomUp' differentiate' . cleanUp