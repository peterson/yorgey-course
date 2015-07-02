module Hanoi where

type Peg  = String
type Move = (Peg, Peg)

-- Move n discs from peg A to peg B
-- (Note: Wikipedia article shows the more common peg A to peg C version!)
--
-- See: https://en.wikipedia.org/wiki/Tower_of_Hanoi#Recursive_solution
--
-- Ex: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
  hanoi (n-1) a c b ++
  [(a,b)] ++
  hanoi (n-1) c b a
