module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  ) where

skips :: [a] -> [[a]]
skips xs = map (`everyNth` xs) [1..(length xs)]

everyNth :: Int -> [a] -> [a]
everyNth n xs = map fst (filter (indexMod n) (zip xs [1..]))

indexMod :: Int -> (a, Int) -> Bool
indexMod num (_, i) = i `mod` num == 0

localMaxima :: [Integer] -> [Integer]
localMaxima = map (snd . fst) . filter isLocalMaxima . getThreeTuple
  where
    getThreeTuple arr = zip (zip arr (drop 1 arr)) (drop 2 arr)
    isLocalMaxima ((a, b), c)  = b > a && b > c

histogram :: [Integer] -> String
histogram = error "Week03.Golf#histogram not implemented"
