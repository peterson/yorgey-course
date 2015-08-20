module AParser where

import           Control.Applicative hiding (Alternative, (<|>))

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

--
-- Ex 1
--

-- apply function f to the result of a parser, and return another parser
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f pa =
  Parser $ \str ->
    case runParser pa str of
      Just (a, str') -> Just (f a , str')
      Nothing        -> Nothing

--
-- Okay, let's try this out ...
--
-- >>> let px = char 'x' -- build a parser for 'x' characters
-- >>> :t px
-- >>> px :: Parser Char
--
-- let's run px on some input
--
-- >>> runParser px "xyz"
-- >>> Just ('x',"yz")
--
-- now, let's look at what mapParser should do ...
-- >>> :t mapParser
-- >>> mapParser :: (a -> b) -> Parser a -> Parser b
--
-- so, it turns a Parser of 'a' into a Parser of 'b', by applying some function f.
--
-- Consider this in-built function ...
-- >>> :t isAlpha
-- >>> isAlpha :: Char -> Bool
--
-- here, a is Char, and b is Bool. We therefor expect mapParser to turn a
-- Parser of 'Char' into a Parser of 'Bool', right?
--
-- >>> :t mapParser (isAlpha) px
-- >>> Parser Bool
--
-- Lets try to 'fmap' over some successfully parsed value (contained in a
-- Parser Char type, and see what we get.
--
-- >>> runParser (mapParser (isAlpha) px) "xyz"
-- >>> Just (True, "yz")
--
-- Yay! The fmap has applied f to the parsed value of "x", to get a True, and it
-- has left the remainder "yz" untouched. It has then wrappered the result back
-- in a Parser b (Bool in this case) type.

-- This is EXACTLY the behaviour we want when we do "fmap" on a Parser.
--
-- fmap :: Functor t => (a -> b) -> t a -> t b.
--
-- In this case, t is Parser, and (a -> b) is the function f that we apply.

instance Functor Parser where
  fmap = mapParser

--
-- and now, with fmap defined, we can use the short-cut syntax of (<$>) (which is
-- just in-fix form of fmap!) instead:
--
-- >>> runParser (isAlpha <$> px) "xyz"
-- >>> Just (True,"yz")


--
-- Ex 2
--

-- Lifts a value into a parser. This just returns a parser which consumes
-- no input (so \str is passed through unchanged in the result) and successfully
-- returns the passed-in value 'a'.
toParser :: a -> Parser a
toParser a = Parser $ \str -> Just (a, str)


-- applyParser returns a parser which first runs parser p1, which will consume
-- some input and produce a result, and THEN passes this function to
-- parser p2, which will do the same. If either p1 or p2 fails, then the whole
-- thing should fail.

applyParser :: Parser (a -> b) -> Parser a -> Parser b
applyParser p1 p2 =
  Parser $ \str ->
    case runParser p1 str of
      Nothing        -> Nothing
      Just (f, str') -> runParser (f <$> p2) str'


instance Applicative Parser where
  pure  = toParser
  (<*>) = applyParser


--
-- Ex 3
--

--
-- expects to see "a", then "b", and returns them as a pair
abParser :: Parser (Char, Char)
abParser =
  (\ x y -> (x,y)) <$> char 'a' <*> char 'b'

-- >>> runParser abParser "abcdef"
-- >>> Just (('a','b'),"cdef")



abParser_ :: Parser ()
abParser_ =
  (\x y -> ()) <$> char 'a' <*> char 'b'

-- >>> runParser abParser_ "abcdef"
-- >>> Just ((),"cdef")



intPair :: Parser [Integer]
intPair =
  (\i1 _ i2 -> [i1,i2]) <$> posInt <*> char ' ' <*> posInt

-- >>> runParser intPair "12 32"
-- >>> Just ([12,32],"")


--
-- Ex 4
--

-- provided with this definition
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = failParser
  (<|>) = choiceOf

failParser :: Parser a
failParser = Parser $ \_ -> Nothing

-- choice operator
choiceOf :: Parser a -> Parser a -> Parser a
choiceOf p1 p2 =
  Parser $ \str ->
    case runParser p1 str of
      Just (a, str') -> Just (a, str')
      Nothing        -> case runParser p2 str of
                          Nothing -> Nothing
                          Just (b, str'') -> Just (b, str'')


--
-- Ex 5
--

-- posInt_ parses an integer and throws it away
posInt_ :: Parser ()
posInt_ =
  (\x -> ()) <$> posInt

-- >>> runParser posInt_ "123dsfd"
-- >>> Just ((),"dsfd")


-- upperChar_ parses a (single) uppercase char and throws it away
upperChar_ :: Parser ()
upperChar_ =
  (\x -> ()) <$> (satisfy isUpper)

-- >>> runParser upperChar_ "Abcdef"
-- >>> Just ((),"bcdef")


intOrUppercase :: Parser ()
intOrUppercase =
  posInt_ <|> upperChar_

-- >>> runParser intOrUppercase "123sfds"
-- >>> Just ((),"sfds")
--
-- >>> runParser intOrUppercase "ABCdef"
-- >>> Just ((),"BCdef")
