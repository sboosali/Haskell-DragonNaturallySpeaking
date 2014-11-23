{-# LANGUAGE FlexibleContexts #-}
-- |
-- wraps "Text.Parsec"
module Commands.Text.Parsec
 ( module Text.Parsec
 , module Commands.Text.Parsec
) where

import Text.Parsec hiding (parse,space,many1)
import qualified Text.Parsec as Parsec
import Data.List.NonEmpty ( NonEmpty(..),toList,  head,tail,last,init )

import Control.Applicative hiding ((<|>), many)
import Data.Functor.Identity


-- | our 'parse'rs are context-sensitive, but the context is passed as argument.
-- i.e. we would want @Reader@ with @local@, if anything, not "non-@local@" @State@.
--
type Parser input output = ParsecT [input] () Identity output

parsing :: Parser token result -> [token] -> Either ParseError result
parsing p = runParser p () ""


-- | 'Parsec.many' is 'NonEmpty' by construction.
-- e.g. I use the 'Foldable' instance of 'NonEmpty' for a safe @foldl1@.
--
many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (NonEmpty a)
many1 p = (:|) <$> p <*> many p


type Word = String

anyWord :: Parser Char Word
anyWord = spaced $ Parsec.many1 $ noneOf separators

word :: String -> Parser Char Word
word = spaced . string

spaced :: Parser Char a -> Parser Char a
spaced = between (many space) (many space)

space :: Parser Char Char
space = char ' ' <?> "space"

whitespaced :: Parser Char a -> Parser Char a
whitespaced = between Parsec.spaces Parsec.spaces

-- | the 'Recognize'r should return only letters
separators :: String
separators = whitespace ++ punctuation
 where
 whitespace = " \t\n\r"
 punctuation = ",;:."

