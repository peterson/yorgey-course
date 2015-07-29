{-# LANGUAGE DeriveFoldable #-}

module Stream where

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
