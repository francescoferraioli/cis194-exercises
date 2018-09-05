module Week02.LogAnalysis
  ( parseMessage
  , parse
  , insert
  , build
  , inOrder
  , whatWentWrong
  , module Week02.Log
  ) where

import Week02.Log
  ( LogMessage(..)
  , MessageTree(..)
  , MessageType(..)
  , TimeStamp
  , testWhatWentWrong
  , testParse
  )

parseMessage :: String -> LogMessage
parseMessage x = case words x of
                  (a:b:c:d) -> case head a of
                    'I' ->  LogMessage Info (read b) (unwords (c : d))
                    'W' ->  LogMessage Warning (read b) (unwords (c : d))
                    'E' ->  LogMessage (Error (read b)) (read c) (unwords d)
                    _   ->  Unknown "This is not in the right format"
                  _ -> Unknown "This is not in the right format"

parse :: String -> [LogMessage]
-- lines :: String -> [String]
-- parseMessage :: String -> LogMessage
-- map :: (String -> LogMessage) -> [String] -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert m@(LogMessage _ t1 _) (Node l nm@(LogMessage _ t2 _) r)
  | t1 < t2 = Node (insert m l) nm r
  | otherwise = Node l nm (insert m r)
insert m _ = Node Leaf m Leaf

buildRev :: [LogMessage] -> MessageTree
buildRev [] = Leaf
buildRev [x] = insert x Leaf
buildRev (x:xs) = insert x (buildRev xs)

build :: [LogMessage] -> MessageTree
build = buildRev . reverse

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf           = []
inOrder (Node l m r)  = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  let
    graveError (LogMessage (Error sev) _ _) = sev >= 50
    graveError _ = False
    getMessageString (LogMessage _ _ mes) = mes
    getMessageString _ = ""
  in
    map getMessageString . inOrder . build . filter graveError

