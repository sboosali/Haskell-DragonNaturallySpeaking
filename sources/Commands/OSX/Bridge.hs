module Commands.OSX.Bridge where

import Text.ParserCombinators.Parsec


begBy parser separator = separator >> parser `sepBy` separator

-- only absolute paths
filePath = fileName `begBy` char '/'
fileName = many (noneOf "/")


parseFilePath :: String -> Either ParseError [String]
parseFilePath = parse filePath ""

