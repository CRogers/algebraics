{-# LANGUAGE PatternSynonyms #-}

module Tree where

import qualified RawTree as RT
import Fix

type Expr = Fix RT.Expr

pattern Sym a = In (RT.Sym a)
pattern Num a = In (RT.Num a)
pattern Add a b = In (RT.Add a b)
pattern Sub a b = In (RT.Sub a b)
pattern Mul a b = In (RT.Mul a b)
pattern Div a b = In (RT.Div a b)
pattern Pow a b = In (RT.Pow a b)

pretty :: Expr -> String
pretty = cata RT.pretty