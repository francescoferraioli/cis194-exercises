module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

import Data.Bool (bool)
import Data.List (unfoldr)
import Data.Tuple (swap)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- toDigits n
--   | n <= 0 = []
--   | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev =
  unfoldr $ \n ->
    if n <= 0
      then Nothing
      else Just (swap (divMod n 10))

-- unfoldr $ bool Nothing . (Just . swap . flip divMod 10) <*> (> 0)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  reverse . zipWith (*) (cycle [1, 2]) . reverse

-- doubleEveryOther =
--   reverse . go . reverse
--     where
--       go [] = []
--       go [a] = [a]
--       go (a : b : cs) = a : (b * 2) : go cs

sumDigits :: [Integer] -> Integer
sumDigits = sum . (toDigits =<<)

validate :: Integer -> Bool
validate =
  (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits
