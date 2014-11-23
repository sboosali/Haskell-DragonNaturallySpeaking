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
 parse context = (Number . read) <$> (spaced $ Parsec.many1 Parsec.digit)


grammarTight = pProduction `parsing` [qq| data Command
ReplaceWith  replace Phrase with Phrase
Click        Times Button click
Undo         undo |]

grammarLoose = pGrammar `parsing` [qq| 

data Command
ReplaceWith  replace Phrase with Phrase
Click        Times Button click
Undo         undo


data Stub
Stub stub


|]


[rule| data Command
ReplaceWith  replace Phrase with Phrase
Click        Times Button click
Undo         undo |]

parseCommand :: String -> Either ParseError Command
parseCommand = parsing (parse def)


-- $ cabal build && cabal exec runhaskell sources/Example/Commands.hs
-- $ cabal exec -- ghc  -outputdir ignore/ignore  -ddump-splices  sources/Example/Commands.hs
main :: IO ()
main = do

 putStrLn ""
 print grammarTight
 print grammarLoose

 putStrLn ""
 print $ ReplaceWith (Words ["this", "and", "that"]) (Words ["that", "and", "this"])
 print $ Click (Number 1) (Number 2)
 print $ Undo

 putStrLn ""
 print $ parseCommand "replace this and that with that and this"
 print $ parseCommand "1 2 click"
 print $ parseCommand "undo"
