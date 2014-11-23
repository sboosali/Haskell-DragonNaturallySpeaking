{-# LANGUAGE FlexibleContexts #-}
-- |
-- wraps "Text.Parsec"
module Commands.Text.Parsec
 ( module Text.Parsec
 , module Commands.Text.Parsec
) where

import Text.Parsec hiding (parse,space,many1,sepBy1,endBy1,sepEndBy1)
import qualified Text.Parsec as Parsec
import Data.List.NonEmpty ( NonEmpty(..),toList,  head,tail,last,init )

import Control.Applicative hiding ((<|>),optional,many)
import Data.Functor.Identity


-- | our 'parse'rs are context-sensitive, but the context is passed as argument.
-- i.e. we would want @Reader@ with @local@, if anything, not "non-@local@" @State@, hence the unit @State@ '()'.
--
type Parser input output = ParsecT [input] () Identity output

-- | 
-- no state, no source, a list stream.
parsing :: Parser token result -> [token] -> Either ParseError result
parsing p = runParser p () ""


-- | 'Parsec.many1' is 'NonEmpty' by construction.
-- e.g. use: elsewhere I use the 'Foldable' instance of 'NonEmpty' for a safe @foldl1@.
--
many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (NonEmpty a)
many1 p = (:|) <$> p <*> many p

-- | 'Parsec.sepBy1' is 'NonEmpty' by construction.
--
-- >>> (digit `sepBy1` char ',') `parsing` "1,2,3"
-- ["1","2","3"]
--
sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
p `sepBy1` sep = (:|) <$> p <*> many (sep *> p)

-- | 'Parsec.endBy1' is 'NonEmpty' by construction.
--
-- defined against 'sepBy1'
endBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
p `endBy1` sep = (p `sepBy1` sep) <* sep

-- | 'Parsec.sepEndBy1' is 'NonEmpty' by construction.
--
-- defined against 'sepBy1'
--
sepEndBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
p `sepEndBy1` sep = (p `sepBy1` sep) <* (optional sep)

-- | 
many1Till :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m (NonEmpty a)
p `many1Till` end = (:|) <$> p <*> (p `manyTill` end)

-- | 
-- 
-- * pro: you can intuitively combine it
-- * con: at the cost of backtracking (one extra run of @p@ at most)
-- 
-- e.g. sacrificing efficiency for clarity, I have a parser that looks like: 
-- 
-- > (p `sepBy1Slow` newline) `sepBy1Slow` (newline *> newline)
-- 
-- this would fail with 'sepBy1' or 'Parsec.sepBy1'. its semantics is: when you see the separator, use the parser.
-- this function semantics is: 'try' to parse the separator only when followed by the parser.
-- instead of writing my own recursive parser, or using 'manyTill' for explicit context.
sepBy1Slow :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
p `sepBy1Slow` sep = (:|) <$> p <*> ps
 where
 ps = try qs <|> pure []
 qs = do
  x <- sep *> p
  xs <- ps
  return (x:xs)


type Word = String

-- |
-- see 'separators'
anyWord :: Parser Char Word
anyWord = spaced $ Parsec.many1 $ noneOf separators

-- | the 'Recognize'r should return only letters
separators :: String
separators = whitespace ++ punctuation
 where
 whitespace = " \t\n\r"
 punctuation = ",;:."

-- |
-- let's you treat words (i.e. 'String's) as "tokens", when your 'Stream' is a 'String', and thus when your real tokens are 'Char's.
word :: String -> Parser Char Word
word = spaced . string

-- |
--
spaced :: Parser Char a -> Parser Char a
spaced = between (many space) (many space)

-- |
-- matches only the space character (overriding 'Parsec.space') 
space :: Parser Char Char
space = char ' ' <?> "space"

-- |
-- any Unicode space character, and the control characters: \t, \n, \r, \f, \v
whitespace :: Parser Char ()
whitespace = Parsec.spaces

