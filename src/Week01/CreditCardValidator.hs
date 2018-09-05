module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

toDigits :: Integer -> [Integer]
toDigits = reverseInt . toDigitsRev

reverseInt :: [Integer] -> [Integer]
reverseInt [] = []
reverseInt (x:xs) = reverseInt xs ++ [x]


toDigitsRev :: Integer -> [Integer]
toDigitsRev l
  | l <= 0 = []
  | otherwise = l `mod` 10 : toDigitsRev (l `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverseInt . doubleEveryOtherL . reverseInt

doubleEveryOtherL :: [Integer] -> [Integer]
doubleEveryOtherL [] = []
doubleEveryOtherL [x] = [x]
doubleEveryOtherL (x:y:zs) = x : (2 * y) : doubleEveryOtherL zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

