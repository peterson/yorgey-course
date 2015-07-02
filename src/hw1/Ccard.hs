module Ccard where

import Data.Char as C

-- Ex: toDigits 1234 == [1,2,3,4]
-- Ex: toDigits 0 == []
-- Ex: toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n | n <= 0 = []
toDigits n | n > 0  = toInteger <$> C.digitToInt <$> show n

fromDigits :: [Integer] -> Integer
fromDigits l = read $ concat $ show <$> l

--
-- Ex: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Ex: doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l =
  zipWith (*) mask l
  where
    mask = reverse $ take (length l) $ cycle [1,2]

-- Ex: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits n = sum $ toDigits $ fromDigits n

-- Ex: validate 4012888888881881 == True
-- Ex: validate 4012888888881882 == False
validate :: Integer -> Bool
validate c =
  rem == 0
  where
    sum = sumDigits $ doubleEveryOther $ toDigits c
    rem = mod sum 10
