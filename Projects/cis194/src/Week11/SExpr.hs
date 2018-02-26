{- CIS 194 HW 11
   due Monday, 8 April
-}

module Week11.SExpr where

import Week11.AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- solution learded from https://github.com/bschwb/cis194-solutions/blob/master/11-applicative-functors-part2/SExpr.hs
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

{-
-- the solution I had first.
zeroOrMore_ :: Parser a -> Parser [a]
zeroOrMore_ p =
  Parser (f [])
  where
    f ary str =
      case runParser pa str of
        Nothing -> Just (ary, str)
        Just (a, str_) -> f (ary++[a]) str_

oneOrMore_ :: Parser a -> Parser [a]
oneOrMore_ p =
  Parser (f [])
  where
    f ary str =
      case runParser pa str of
        Nothing -> case ary of
                    [] -> Nothing
                    otherwise -> Just (ary, str)
        Just (a, str_) -> f (ary++[a]) str_

*AParser> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
Just ("ABC","dEfgH")
*AParser> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
Just ("ABC","dEfgH")
*AParser> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
Just ("","abcdeFGh")
*AParser> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
Nothing
-}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ char ' '

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

{-
*AParser> runParser ident "foobar baz"
Just ("foobar"," baz")
*AParser> runParser ident "foo33fA"
Just ("foo33fA","")
*AParser> runParser ident "2bad"
Nothing
*AParser> runParser ident ""
Nothing
-}

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

parseAtomIdent :: Parser Atom
parseAtomIdent = I <$> ( spaces *> ident <* spaces )

parseAtomInt :: Parser Atom
parseAtomInt = N <$> ( spaces *> posInt <* spaces )

parseAtom :: Parser Atom
parseAtom = parseAtomInt <|> parseAtomIdent

parseSExprAtom :: Parser SExpr
parseSExprAtom =
    A <$> ( parseAtom
            <|> (spaces *> ((char '(') *> parseAtom <* (char ')')) <* spaces) )

parseSExprComb :: Parser SExpr
parseSExprComb =
    Comb <$>
        (spaces *>
            ((char '(') *> oneOrMore parseSExpr <* (char ')'))
        <* spaces)

parseSExpr :: Parser SExpr
parseSExpr = (parseSExprAtom <|> parseSExprComb)

