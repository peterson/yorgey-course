module Fibonacci where

import Data.Function (fix)
import qualified Data.Map as M

--
-- Ex 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- rely on pattern-match ordering here, rather than explicit guards e.g. | n >= 1 etc ...

-- non-memoized
fibs1 :: [Integer]
fibs1 = map fib [0..] -- note: infinite list!


--
-- Ex 2

-- memoized
fibs2 :: [Integer]
fibs2 = map mfib [0..]

fib' :: (Int -> Integer) -> Int -> Integer
fib' f 0 = 0
fib' f 1 = 1
fib' f n = f (n-2) + f (n-1)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

mfib :: Int -> Integer
mfib = fix (memoize . fib')
