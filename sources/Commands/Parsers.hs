module Commands.Parsers where

import Text.Parsec

import Prelude hiding (until)
import Control.Applicative ((<$>), (<*>), (<*), (*>))


parseObjC :: String -> Either ParseError ([String], String)
parseObjC = parse objcFile ""

-- | extracts the imports and the implementation/interface code from a simple ".m" or ".h" file
objcFile = (,) <$> (many inclusion <* spaces) <*> body

-- | crude parser: fails against whitespace
inclusion
 =   (try $ everythingBetweenExclusively (string "#import ")  (char '\n'))
 <|> (try $ everythingBetweenExclusively (string "#include ") (char '\n'))

-- | crude parser: fails against comments and strings
body
 =   (try $ everythingBetweenInclusively (string "@implementation") (string "@end"))
 <|> (try $ everythingBetweenInclusively (string "@interface")      (string "@end"))

everythingBetweenExclusively open close = between            open close (until close) 
everythingBetweenInclusively open close = betweenInclusively open close (until close) 

-- | 'between' is exclusive
betweenInclusively open close parser = (+++) <$> open <*> parser <*> close
 where (+++) x y z = x ++ y ++ z

-- | 
--
-- 'try' doesn't consume on failure and consumes on success
--
-- 'lookAhead' doesn't consume on success and errors on failure
until parser = anyChar `manyTill` ((try . lookAhead) parser)

-- $ cabal exec runhaskell sources/Commands/Parsers.hs
main = do
 parseObjC <$> readFile "actor.m" >>= print
