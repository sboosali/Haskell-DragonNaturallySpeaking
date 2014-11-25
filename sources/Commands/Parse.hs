{-# LANGUAGE ExistentialQuantification, Rank2Types, FlexibleContexts #-}
module Commands.Parse where
import Commands.Text.Parsec
import Commands.Generic

import Data.Functor


-- | a 'Parse'able type. 
--
-- 'parse' is vaguely like @readPrec@ but:
--
-- * the input is a natural-language string, not a Haskell-code string
-- * if it had laws, it might be like @parse . prettyPrint = id@, not @read . show = id@.
-- 
--
-- parses an unusable\/unstructured 'Char'acter list, into a usable\/structured syntax tree.
-- 
--
class Parse r where
 parse :: ParsingContext -> Parser Char r

-- | existentially quantified constructor, without some scoped-over function.
--
-- i.e. the result of the parser must be ignored.
data ParsingContext = forall a. ParsingContext (Parser Char a)

-- | the 'Default' 'ParsingContext' uses 'eof'.
-- 
-- i.e. lets a "hungry rule" (e.g. "Commands.Rule.Type") "be satiated" by (i.e. parse successfully) the end of file stops it (successfully), by default, when there's no ending context.
instance Default ParsingContext where
 def = ParsingContext eof

-- |
--
contextual :: Parser Char a -> ParsingContext
contextual = ParsingContext

-- | 'manyUntil' ignores the result of the second argument.
--
-- i.e. 'undefined' is ignored
--
manyUntil :: Parser Char a -> ParsingContext -> Parser Char [a]
manyUntil parser (ParsingContext context) = parser `manyTill` ((try . lookAhead) context <|> (undefined <$ eof))

