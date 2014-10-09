module Commands.OSX.Bridge where

import Text.ParserCombinators.Parsec


-- | like 'endBy', but with leading separator, not trailing
begBy parser separator = separator >> parser `sepBy` separator

-- | only absolute paths i.e. "@/...@" no "@~/...@" or "@./...@" or "@...@"
filePath = fileName `begBy` char '/'
fileName = many (noneOf "/")

parseFilePath :: String -> Either ParseError [String]
parseFilePath = parse filePath ""

