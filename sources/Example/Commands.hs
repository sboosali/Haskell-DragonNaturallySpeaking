{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Example.Commands where
import Commands.Etc
import Commands.TH
import Commands.TH.Syntax
import Commands.Generic
import Commands.Text.Parsec
import Commands.Parse
import Commands.Grammar

import Control.Lens
import qualified Text.Parsec as Parsec
import Text.InterpolatedString.Perl6

import Control.Applicative hiding (many,(<|>))
import Control.Exception


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

grammarTight :: Possibly Production
grammarTight = pProduction `parseThrow` [qq| data Command
ReplaceWith  replace Phrase with Phrase
Undo         undo |]

grammarLoose :: Possibly Grammar
grammarLoose = pGrammar `parseThrow` [qq| 

data Command
ReplaceWith  replace Phrase with Phrase
Undo         undo

data Stub
Stub stub


|]

grammarWrong :: Possibly Grammar
grammarWrong = pGrammar `parseThrow` [qq| 
data Stub
Stub st'ub
|]


[rule| data Command
ReplaceWith    replace Phrase with Phrase
Click          Times Button click
TypeSignature  has type Phrase
Undo           undo |]

-- Undo           un'do |]
-- reports error, with file's (not template's) line/column

parseCommand :: String -> Possibly Command
parseCommand = parseThrow (parse def)


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
 print =<< grammarTight
 print =<< grammarLoose
 ((print =<< grammarWrong) `catch` (\ (e :: ParseError) -> print "CAUGHT" >> print e)) >> print "AFTER"

 putStrLn ""
 print $ ReplaceWith (Words ["this", "and", "that"]) (Words ["that", "and", "this"])
 print $ Click (Number 1) (Number 2)
 print $ TypeSignature (Words ["maybe", "a"])
 print $ Undo

 putStrLn ""
 print =<< parseCommand "replace this and that with that and this"
 print =<< parseCommand "1 2 click"
 print =<< parseCommand "has type maybe a"
 print =<< parseCommand "undo"

 putStrLn ""
 let grammar' = grammar (undefined :: Command)
 print grammar'

 putStrLn ""
 print $ grammar' ^.start
 print $ grammar' ^..terminals
 print $ grammar' ^..nonTerminals

 putStrLn ""
 print $ getHoles grammar'
 print $ getParts grammar'
