module Calc where

import ExprT
import Parser

--
-- Ex 1

eval :: ExprT -> Integer
eval (Lit v) = v
eval (Add a b) = (+) (eval a) (eval b)
eval (Mul a b) = (*) (eval a) (eval b)


--
-- Ex 2

evalStr :: String -> Maybe Integer
evalStr s =
  case parseExp Lit Add Mul s of
    Nothing -> Nothing
    Just p  -> Just (eval p)

--
-- Ex 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id


--
-- Ex 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  (MinMax a) `add` (MinMax b) = MinMax (max a b)
  (MinMax a) `mul` (MinMax b) = MinMax (min a b)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit a = Mod7 (flip mod 7 a)
  (Mod7 a) `add` (Mod7 b) = Mod7 (flip mod 7 $ a + b)
  (Mod7 a) `mul` (Mod7 b) = Mod7 (flip mod 7 $ a * b)
