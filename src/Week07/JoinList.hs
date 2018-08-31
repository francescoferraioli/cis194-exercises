{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Week07.JoinList
  ( JoinList(..)
  , joinListToList
  , (!!?)
  , (+++)
  , indexJ
  , dropJ
  , takeJ
  , scoreLine
  , tag
  , main
  ) where

import Week07.Buffer (Buffer(..))
import Week07.Editor (editor, runEditor)
import Week07.Scrabble (Score(..))
import Week07.Sized (Sized(..), Size(..), getSize)

import Data.Monoid

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq, Show)

joinListToList :: JoinList m a -> [a]
joinListToList Empty = []
joinListToList (Single _ a) = [a]
joinListToList (Append _ l r) = joinListToList l ++ joinListToList r

(!!?) :: Int -> [a] -> Maybe a
(!!?) n = lookup n . zip [0 ..]

--------------------------- Exercise 1

-- Suggestion (no tests):
-- Pulls the monoidal value out of the root of the JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

--------------------------- Exercise 2

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ v)
    | i == 0 = Just v
    | otherwise = Nothing
indexJ i (Append _ l r)
    | i < ln = indexJ i l
    | i - ln < rn = indexJ (i - ln) r
    | otherwise = Nothing
    where 
      ln = tagSize l
      rn = tagSize r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n x@(Single _ _)
    | n > 0 = Empty
    | otherwise = x
dropJ n x@(Append _ l r)
    | n >= ln + rn = Empty
    | n >= ln = dropJ (n - ln) r
    | n > 0 = dropJ n l +++ r
    | otherwise = x
    where 
      ln = tagSize l
      rn = tagSize r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n x@(Single _ _)
    | n > 0 = x
    | otherwise = Empty
takeJ n x@(Append _ l r)
    | n >= ln + rn = x
    | n >= ln = l +++ takeJ (n - ln) r
    | n > 0 = takeJ n l
    | otherwise = Empty
    where 
      ln = tagSize l
      rn = tagSize r

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine = error "Week07.JoinList#scoreLine not implemented"

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString = error "Week07.JoinList#toString not implemented for Buffer (JoinList (Score, Size) String)"

  fromString :: String -> JoinList (Score, Size) String
  fromString = error "Week07.JoinList#fromString not implemented for Buffer (JoinList (Score, Size) String)"

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = error "Week07.JoinList#line not implemented for Buffer (JoinList (Score, Size) String)"

  replaceLine ::
       Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine = error "Week07.JoinList#replaceLine not implemented for Buffer (JoinList (Score, Size) String)"

  numLines :: JoinList (Score, Size) String -> Int
  numLines = error "Week07.JoinList#numLines not implemented for Buffer (JoinList (Score, Size) String)"

  value :: JoinList (Score, Size) String -> Int
  value = error "Week07.JoinList#value not implemented for Buffer (JoinList (Score, Size) String)"

initialValue :: JoinList (Score, Size) String
initialValue = error "Week07.JoinList#initialValue not implemented"

main :: IO ()
main =
  runEditor editor initialValue
