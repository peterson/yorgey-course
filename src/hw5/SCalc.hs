{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SCalc where

import Calc
import StackVM
import Parser

--
-- Ex 5

instance Expr Program where
  lit a     = [PushI a]
  a `add` b = a ++ b ++ [Add]
  a `mul` b = a ++ b ++ [Mul]


testProgram :: Maybe Program
testProgram = parseExp lit add mul "(3 * -4) + 5"

-- compile a program
compile :: String -> Maybe Program
compile = parseExp lit add mul

-- run a compiled program
run :: Maybe Program -> Either String StackVal
run Nothing  = Left "run: error: program did not compile"
run (Just p) = stackVM p

-- compile and run
comprun :: String -> Either String StackVal
comprun = run . compile
