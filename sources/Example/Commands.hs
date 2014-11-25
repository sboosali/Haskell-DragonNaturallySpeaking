{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Example.Commands where
import Example.Types
import Commands.Text.Parsec
import Commands.TH
import Commands.Parse
import Commands.Generic

import qualified Text.Parsec as Parsec
import Text.InterpolatedString.Perl6

import Control.Applicative hiding (many,(<|>))
import Language.Haskell.TH


-- stubs
type Phrase = Words
type Times = Number
type Button = Number

newtype Words = Words [String] deriving (Show,Eq)
newtype Number = Number Integer deriving (Show,Eq)

instance Parse Words where
 parse context = Words <$> anyWord `manyUntil` context

instance Parse Number where
 parse _ = (Number . read) <$> (spaced $ Parsec.many1 Parsec.digit)


grammarTight = pProduction `parsing` [qq| data Command
ReplaceWith  replace Phrase with Phrase
Undo         undo |]


grammarLoose = pGrammar `parsing` [qq| 

data Command
ReplaceWith  replace Phrase with Phrase
Undo         undo

data Stub
Stub stub


|]


[rule| data Command
ReplaceWith    replace Phrase with Phrase
Click          Times Button click
TypeSignature  has type Phrase
Undo           undo |]

parseCommand :: String -> Either ParseError Command
parseCommand = parsing (parse def)


-- | tests:
--
-- * arbitrary whitespace
-- * multiple productions
--
-- and:
-- * adjacent non-terminals
-- * adjacent terminals
-- * suffix operator and prefix operator
-- * single word
-- 
-- $ cabal build && cabal exec runhaskell sources/Example/Commands.hs
--
-- $ cabal exec -- ghc  -outputdir ignore/ignore  -ddump-splices  sources/Example/Commands.hs
--
main :: IO ()
main = do

 putStrLn ""
 print grammarTight
 print grammarLoose

 putStrLn ""
 print $ ReplaceWith (Words ["this", "and", "that"]) (Words ["that", "and", "this"])
 print $ Click (Number 1) (Number 2)
 print $ TypeSignature (Words ["maybe", "a"])
 print $ Undo

 putStrLn ""
 print $ parseCommand "replace this and that with that and this"
 print $ parseCommand "1 2 click"
 print $ parseCommand "has type maybe a"
 print $ parseCommand "undo"
