{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Example.Commands where
import Commands.Etc
import Commands.TH
import Commands.TH.Syntax
import Commands.Generic
import Commands.Text.Parsec
import Commands.Parse
import Commands.Recognize
import Commands.Grammar

import Control.Lens
import qualified Text.Parsec as Parsec
import Text.InterpolatedString.Perl6

import Data.Tree
import Control.Applicative hiding (many,(<|>))
import Control.Exception
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


instance Recognize NatLink Phrase where
 recognize _ = Node (NatLink ''Phrase "<dgndictation>") []

instance Recognize NatLink Number where
 recognize _ = Node (NatLink ''Number "TODO") []


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


-- [rule| data Command
-- ReplaceWith    replace Phrase with Phrase
-- Click          Times Button click
-- TypeSignature  has type Phrase
-- Undo           undo |]


[rule| data Command
ReplaceWith    replace Words with Words
Click          Number Number click
TypeSignature  has type Words
Undo           undo |]


-- Undo           un'do |]
-- reports error, with file's (not template's) line/column

parse_Command :: String -> Possibly Command
parse_Command = parseThrow (parse def)

recognize_Command :: String
recognize_Command = serializeNatLinkGrammar (recognize (undefined :: Command) :: Tree NatLink)

rule_Command :: Grammar
rule_Command = grammar (undefined :: Command)


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
 ((print =<< grammarWrong) `catch` (\ (e :: ParseError) -> putStrLn "CAUGHT ParseError..." >> print e))

 putStrLn ""
 print $ ReplaceWith (Words ["this", "and", "that"]) (Words ["that", "and", "this"])
 print $ Click (Number 1) (Number 2)
 print $ TypeSignature (Words ["maybe", "a"])
 print $ Undo

 putStrLn ""
 print =<< parse_Command "replace this and that with that and this"
 print =<< parse_Command "1 2 click"
 print =<< parse_Command "has type maybe a"
 print =<< parse_Command "undo"

 putStrLn ""
 print rule_Command

 putStrLn ""
 print $ rule_Command ^.start
 print $ rule_Command ^..terminals
 print $ rule_Command ^..nonTerminals

 putStrLn ""
 print $ getStart rule_Command
 print $ getHoles rule_Command
 print $ getParts rule_Command

 putStrLn ""
 putStrLn recognize_Command
 putStrLn ""
