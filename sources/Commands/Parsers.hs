{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Commands.Parsers where
import Commands.Generic
import Commands.Etc

import Text.Parsec hiding (parse)

import Prelude hiding ()
import Control.Applicative hiding ((<|>), many)
import Data.Char


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
 parse :: Context -> Parser r

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


anyWord :: Parser Word
anyWord = spaced $ many1 $ noneOf " \t\n\r"

word :: String -> Parser Word
word = spaced . string

spaced :: Parser Word -> Parser Word
spaced = between spaces spaces

type Part = String
type Context = [Part]
type Parser output = Parsec String () output
type Words = String
type Word = String
type Times = Number
type Button = Number

parseCommand :: Words -> Either ParseError Command
parseCommand = runParser (parse []) () ""


data ConstructorSyntax = RawConstructor | MixFixConstructor [MixFixSyntax] | NatLangConstructor
 deriving (Show)
data MixFixSyntax = Part [String] | Hole
 deriving (Show)

parseConstructor :: String -> Possibly ConstructorSyntax
parseConstructor ('X':name) = either (fail . show) (return . MixFixConstructor) $ parseMixFix `parsing` name
-- parseConstructor (:name) = NatLangConstructor []

-- this is a really long sentence to trigger a mind that after 80 characters okay more characters we need more characters why isn't it wrapping I don't know

parseMixFix :: Parser [MixFixSyntax]
parseMixFix = many1 $
     ((Part . parsed unCamelCase) <$> (many1 $ noneOf "_")
 <|> (Hole <$ char '_'))

unCamelCase :: Parser [String] 
unCamelCase = (:) <$> many1 lower <*> unClassCase
 <|> ([] <$ eof)

unClassCase :: Parser [String]
unClassCase = (:) <$> ((:) <$> toLower `fmap` upper <*> many lower) <*> unClassCase
 <|> ([] <$ eof)
 -- fmap :: (Char -> Char) -> (Parser _ Char -> Parser _ Char)
 -- @toLower `fmap` upper@ returns a @lower@ after parsing an @upper@

-- parsing :: ParsecT x u m y -> ParsecT y u m z -> ParsecT x u m z
parsed :: (Default o) => Parser o -> String -> o
parsed p = either (const def) id . runParser p () ""
-- I don't like this, it should fail
-- and the parser should include the other parser, not call it

parsing :: Parser o -> String -> Either ParseError o
parsing p s = runParser p () "" s


-- $ cabal exec runhaskell sources/Commands/Parsers.hs
main :: IO ()
main = do
 -- parseTest (word "with") " with "
 -- parseTest (word "with")  "with "
 -- parseTest (word "with") " with"
 -- parseTest (word "with")  "with"
 -- parseTest (anyWord `manyTill` try (word "with")) " this and that with that "

 putStrLn ""
 print $ parseCommand
  "replace this and that with that and this"
 print $ parseCommand
  "1 2 click"
 print $ parseCommand
  "replace  double greater equals  with  backtick camel lit double lit greater lit equals backtick"

 putStrLn ""
 parseTest parseMixFix "directionsFrom_to_" -- == [Part ["directions","from"], Hole, Part ["to"], Hole]
 parseTest parseMixFix "__click" -- == [Hole, Hole, Part ["click"]]

 putStrLn ""
 parseTest unCamelCase "unCamelCase"
