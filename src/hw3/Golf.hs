module Golf where

import Data.List as L
import Data.Map as M

--
--
-- The stated goal in the homework is to write code that is 'as short as
-- possible' (excluding type signatures, type definitions and supporting
-- comments). Whilst this is indeed measurable, personally I don't see this
-- as a particularly laudable goal. Instead, it is my aim here to write code
-- that is as 'readable' as possible, i.e. clear for a beginning Haskell
-- programmer to understand, whilst also being 'short', where practical.
--
--


--
-- Ex 1: Hopscotch
--

-- Output of 'skips' is a list of lists. First list in output should be
-- the input list. Second list in output should contain every second
-- element of the input list. The n'th list in output should contain every n'th
-- element of the input list.
--
-- Ex:
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []

skips :: [a] -> [[a]]
skips l =
  [skip n l | n <- [1..(length l)]]
  where
    skip n l = [l !! (k-1) | k <- [n..(length l)], (k `mod` n == 0)]

--
-- Ex 2: Local maxima
--
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it.
--
-- Ex:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima l =
  [l !! k | k <- [1..((length l)-2)], l!!k > l!!(k-1), l!!k > l!!(k+1)]


--
-- Ex 3: Histogram
--
-- Valid input values are [0..9]
--
-- Ex:
--
-- histogram [1,1,1,5] ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789
--
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789

histogram :: [Integer] -> String
histogram = undefined

--
-- note: [Int] used as return type as length :: [a] -> Int

hist :: [Integer] -> [Int]
hist l =
  [(look k mapped)| k <- [0..9]]
  where
    mapped   = fromList $ zip (head <$> grouped) (length <$> grouped)
    grouped  = L.group $ L.sort l
    look k m =
      case (M.lookup k m) of
        (Just v) -> v
        Nothing  -> 0


transpose :: [String] -> [String]
transpose = undefined

flatten :: [String] -> String
flatten = undefined

label :: [String]
label = undefined
