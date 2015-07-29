{-# LANGUAGE DeriveFoldable #-}

module Streams where

import Data.Bits

--
-- Ex 3
--


data Stream a =
  a :. Stream a    -- (:.) is constructor (in infix position) taking a and (Stream a)
  deriving (Eq, Ord, Foldable)

instance Show a => Show (Stream a) where
  show = show . (take 20) . streamToList

--
-- Converts stream to list
--
streamToList :: Stream a -> [a]
streamToList = foldr (:) []


--
-- Ex 4
--


--
-- streamRepeat
--
-- Ex
-- >>> streamRepeat 1
-- >>> [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
--
streamRepeat :: a -> Stream a
streamRepeat a = a :. (streamRepeat a)


--
-- streamMap
--
-- Ex
-- >>> streamMap (*2) (streamRepeat 1)
-- >>> [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]
--
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a :. s) = f a :. (streamMap f s)


--
-- streamFromSeed
--
-- Ex
-- >>> streamFromSeed (+1) 1
-- >>> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a :. (streamFromSeed f (f a))


--
-- Ex 5
--

nats :: Stream Integer
nats = streamFromSeed (+1) 0

oddNats   = streamFromSeed (+2) 1   -- odd naturals
evenNats0 = streamFromSeed (+2) 0   -- even naturals including 0
evenNats  = tailStream evenNats0    -- even naturals excluding 0

zeros     = streamRepeat 0          -- a stream of 0's
ones      = streamRepeat 1          -- a stream of 1's

--
-- helpers
--

tailStream :: Stream a -> Stream a
tailStream (_:.s) = s

headStream :: Stream a -> a
headStream (a:._) = a

lg = logBase 2

--
-- interleaveStreams
--
-- Ex
-- >>> interleaveStreams (streamRepeat 0) (streamRepeat 1)
-- >>> [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]
--
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a :. as) (b :. bs) = a :. (b :. (interleaveStreams as bs))


--
-- ruler
--
-- A stream where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n.
--
-- Sequence is: [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...]

ruler :: Stream Integer
ruler = interleaveStreams as bs
  where
    as = zeros
    bs = streamMap rule evenNats
    rule = floor . lg . fromIntegral . gtbinfac
    -- note: it's "safe" to convert fromIntegral and back (via floor) as
    -- values returned by gtbinfac will always be integer powers 2^n


--
-- gtbinfac
--
-- Returns the integer which is the greatest binary factor for the integer x.
-- A binary factor is a number which is 2^n for some n,
-- i.e. 1, 2, 4, 8, 16, 32, 64, ...
--
-- How it works:
--
-- This function makes use of a bitwise-and operation on the binary representation
-- of the integer, which is provided in Data.Bits included in GHC base.
--
-- For any 2's complement binary representation, the operation "x & (~x+1)" will
-- return a value that represents the most significant bit (MSB) that was set in
-- the binary representation of x.
--
-- For more, see here: https://stackoverflow.com/questions/1551775
--
-- 1. Note that (~x+1) is the safe definition of ~x for a 2's complement binary
-- representation.
--
-- Ex.
-- >>> gtbinfac 8
-- >>> 8
-- ... as 8 is evenly dvisible by 8 (= 2 ^ 3)
--
-- >>> gtbinfac 12
-- >>> 4
-- ... as 12 is evenly divisible by 4 (= 2 ^ 2)
--
-- >>> gtbinfac 7
-- >>> 1
-- ... as 7 is evenly divisible by 1 (= 2 ^ 0) ... like all odd numbers!

gtbinfac :: Integer -> Integer
gtbinfac x = (x .&. ((complement x) + 1))
