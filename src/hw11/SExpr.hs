{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isSpace)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

--
-- either parse a character, or return [] in the failure case
--
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure [] -- note, order is important!

--
-- parses a character, and then attempts to parse another (which may fail)
--
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

--
-- parse whitespace
--
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

--
-- Parses an identifier, which is defined to be an alphabetic char followed by
-- zero or more alphanumeric characters.
--
ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


--
-- Parse s-expressions
--
-- An s-expression is defined here as either an atom or a list of
-- s-expressions. An s-expression can start with any number of leading
-- or trailing spaces, which need to be thrown away.
--
parseSExpr :: Parser SExpr
parseSExpr =
  spaces *> (atom <|> sexp) <* spaces
  where
    atom = liftA A parseAtom
    sexp = liftA Comb $ char '(' *> oneOrMore parseSExpr <* char ')'

--
-- Parse atoms
--
-- An atom is defined as an identifier (lit) or an integer (int) value
parseAtom :: Parser Atom
parseAtom = int <|> lit
  where
    int = liftA N posInt
    lit = liftA I ident
