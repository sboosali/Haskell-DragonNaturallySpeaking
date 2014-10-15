module Commands.Parsers where

import Filesystem.Path.CurrentOS
import Filesystem.Path
import Text.Parsec

import Prelude hiding (until)


parseObjC :: String -> Either ParseError ([String], String)
parseObjC = parse objcFile ""

-- | extracts the imports and the implementation/interface code from a simple ".m" or ".h" file
objcFile = do
 inclusions <- many inclusion
 spaces
 body' <- body
 return (inclusions, body')

-- | crude parser: fails against whitespace
inclusion
 =   (everythingBetweenExclusively (string "#import ")  (char '\n'))
 <|> (everythingBetweenExclusively (string "#include ") (char '\n'))

-- | crude parser: fails against comments and strings
body
 =   (everythingBetweenInclusively (string "@implementation") (string "@end"))
 <|> (everythingBetweenInclusively (string "@interface")      (string "@end"))

everythingBetweenExclusively open close = try $ between open close (until close) 

everythingBetweenInclusively open close = try $ betweenInclusively open close (until close) 

-- | 'between' is exclusive
betweenInclusively open close parser = do
 x <- open
 y <- parser
 z <- close
 return $ x ++ y ++ z

-- | 
--
-- 'try' doesn't consume on failure and consumes on success
--
-- 'lookAhead' doesn't consume on success and errors on failure
until parser = anyChar `manyTill` ((try . lookAhead) parser)

