module Commands.Parsers where

import Text.Parsec hiding (parse)

import Prelude hiding ()
import Control.Applicative hiding ((<|>))


data Command
 = Xreplace_with_ Phrase Phrase
 | X__click (Maybe Times) (Maybe Button)
 deriving (Show)

data Phrase = Dictation [Word] | Join Joiner Phrase | Xlit_ Word -- | Xquote_unquote [Word]
 deriving (Show)
data Joiner = Camel | Snake
 deriving (Show)
newtype Number = Number Integer
 deriving (Show)


class Rule r where
 parse :: Context -> Parser Words r

instance Rule Command where
 parse context
  =   try (Xreplace_with_ <$> (word "replace"
                            *> parse ("with":context) <*
                               word "with")
                          <*>  parse context)

  <|> try (X__click       <$>  parse ("click":context)
                          <*> (parse ("click":context)
                            <* word "click"))

instance Rule Phrase where
 parse context
  = Dictation <$> anyWord `manyTill` (lookAhead anyPart <|> (undefined <$ eof))
  -- 'lookAhead' never consumes input, versus 'try' which consumes input on success
  -- we should group all the words until a @part@ of the parent, hence the @anyPart@
  -- we should group all the words until the end, hence the 'eof'
  -- the types of the left and right of '(<|>)' must match, hence the @undefined <$@
  -- the results of the the right of 'manyTill' is ignored, hence @undefined@
  -- 'eof' never consumes input, hence no 'try'
   where anyPart = choice $ map (try . word) context
   -- @part@s can share prefixes, hence the 'try'

instance Rule Joiner where
 parse _
  =   try (Camel <$ word "camel")
  <|> try (Snake <$ word "snake")

instance Rule Number where
 parse _ = Number . read <$> spaced (many1 digit)

instance (Rule r) => Rule (Maybe r) where
 parse = optionMaybe . try . parse

instance (Rule r) => Rule [r] where
 parse context = many1 $ try $ parse context


anyWord :: Parser Words Word
anyWord = spaced $ many1 $ noneOf " \t\n\r"

word :: String -> Parser Words Word
word = spaced . string

spaced = between spaces spaces

type Part = String
type Context = [Part]
type Words = String
type Word = String
type Parser input output = Parsec input () output
type Times = Number
type Button = Number

parseCommand :: Words -> Either ParseError Command
parseCommand = runParser (parse []) () ""


-- $ cabal exec runhaskell sources/Commands/Parsers.hs
main = do
 -- parseTest (word "with") " with "
 -- parseTest (word "with")  "with "
 -- parseTest (word "with") " with"
 -- parseTest (word "with")  "with"
 -- parseTest (anyWord `manyTill` try (word "with")) " this and that with that "
 print $ parseCommand
  "replace this and that with that and this"
 print $ parseCommand
  "1 2 click"
 print $ parseCommand
  "replace  double greater equals  with  backtick camel lit double lit greater lit equals backtick"
