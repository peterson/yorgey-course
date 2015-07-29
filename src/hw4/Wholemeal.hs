module Wholemeal where

--
-- Ex 1: Wholemeal programming
--
-- Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1’ and fun2’ respectively.
--
-- Hint: For this problem you may wish to use the functions iterate
-- and takeWhile


--
-- initial version

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


--
-- wholemeal version

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)


--
-- initial version

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


--
-- wholemeal version
-- TODO
fun2' :: Integer -> Integer
fun2' = undefined


--
-- Ex 2: Folding with trees
--

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

--
-- Write a function foldTree, which generates a balanced binary tree from
-- a list of values, using foldr.
--
-- Ex:
-- foldTree "ABCDEFGHIJ" ==
--   Node 3
--     (Node 2
--       (Node 0 Leaf ’F’ Leaf)
--       ’I’
--       (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--     ’J’
--     (Node 2
--       (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--       ’H’
--       (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
--

foldTree :: [a] -> Tree a
foldTree l = undefined
