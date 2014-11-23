{-# LANGUAGE ExistentialQuantification, Rank2Types, FlexibleContexts #-}
module Commands.Parse where
import Commands.Text.Parsec
import Commands.Generic

import Data.Functor


-- |
class Parse r where
 parse :: ParsingContext -> Parser Char r


-- | existentially quantified, without some scoped-over function. i.e. the result of the parser must be ignored.
data ParsingContext = forall a. ParsingContext (Parser Char a)

-- |
instance Default ParsingContext where
 def = ParsingContext eof

contextual :: Parser Char a -> ParsingContext
contextual = ParsingContext

-- | 'manyTill' ignores the result of the second argument. i.e. 'undefined' is ignored
manyUntil :: Parser Char a -> ParsingContext -> Parser Char [a]
manyUntil parser (ParsingContext context) = parser `manyTill` ((try . lookAhead) context <|> (undefined <$ eof))


