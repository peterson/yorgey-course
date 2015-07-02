{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--
--
-- Parser
--
--

--
-- parse a single line from the log
--
-- Ex:
--   parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
--   parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
--   parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage s =
  case h of
    "I" -> parseInfoMessage rest
    "W" -> parseWarningMessage rest
    "E" -> parseErrorMessage rest
    _ -> Unknown s
  where
    (h:t) = words s
    rest  = unwords t

parseInfoMessage :: String -> LogMessage
parseInfoMessage s =
  LogMessage Info (read tm) (unwords rst)
  where
    (tm:rst) = words s

parseWarningMessage :: String -> LogMessage
parseWarningMessage s =
  LogMessage Warning (read tm) (unwords rst)
  where
    (tm:rst) = words s

parseErrorMessage :: String -> LogMessage
parseErrorMessage s =
  LogMessage (Error (read errno)) (read tm) (unwords rst)
  where
    (errno:tm:rst) = words s

--
-- parse a whole log file
parse :: String -> [LogMessage]
parse l =
  parseMessage <$> lines l


--
--
-- MessageTree
--
--

node :: LogMessage -> MessageTree
node m = Node Leaf m Leaf

--
-- note: assuming timestamp values are unique (i.e. no two messages have the
-- same timestamp value .. hence not using <= or => in patterns, just < and > !)
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m (Leaf) = node m
insert m (Node Leaf n r)
  | ts m < ts n = Node (node m) n r
insert m (Node l n Leaf)
  | ts m > ts n = Node l n (node m)
insert m (Node l n r)
  | ts m < ts n = Node (insert m l) n r
  | otherwise   = Node l n (insert m r)

-- extract timestamp from LogMessage
ts :: LogMessage -> TimeStamp
ts (Unknown _) = error "undefined"
ts (LogMessage _ t _) = t

--
-- Flipping the order of argments in 'insert' gives a function
-- that can be used by foldl to build the tree!
-- i.e. (flip insert) :: MessageTree -> LogMessage -> MessageTree !
--
-- In general foldl is to be avoided and foldl' (strict) used instead.

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

--
-- Perform an in-order traversal of the MessageTree, which should already
-- be sorted by virtue of the behaviour of the 'insert' function. This emits
-- a sorted list [LogMessage].
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node l n r) = inOrder l ++ [n] ++ inOrder r


--
-- Take an unsorted list [LogMessage] and give back a sorted list [String],
-- of Error's with severity > 50, discarding any other LogMessage's.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms =
  message <$> sorted
  where
    important = filter isImportantErrorMessage ms
    sorted = inOrder $ build important

-- Extracts the 'message' String from LogMessage
message :: LogMessage -> String
message (LogMessage _ _ s) = s
message (Unknown s) = s

--
-- Any error message i.e. with severity > 0
isErrorMessage :: LogMessage -> Bool
isErrorMessage =
  isErrorMessageWithSev 0

--
-- 'Important' defined as severity > 50.
isImportantErrorMessage :: LogMessage -> Bool
isImportantErrorMessage =
  isErrorMessageWithSev 50

--
-- generic predicate
isErrorMessageWithSev :: Int -> LogMessage -> Bool
isErrorMessageWithSev _ (Unknown _) = False
isErrorMessageWithSev s (LogMessage t _ _) =
  case t of
    (Error sev) -> (sev > s)
    _ -> False
