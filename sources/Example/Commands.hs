{-# LANGUAGE QuasiQuotes #-}
module Example.Commands where
import Example.Types
import Commands.Text.Parsec
import Commands.TH

import qualified Text.Parsec as Parsec
import Text.InterpolatedString.Perl6

import Control.Applicative hiding (many,(<|>))
import Language.Haskell.TH


type Phrase = Words
type Times = Number
type Button = Number

newtype Words = Words [String] deriving (Show,Eq)
newtype Number = Number Integer deriving (Show,Eq)


grammar = many1 pProduction `parsing` [qq| data Command
ReplaceWith  replace Phrase with Phrase
Click        Times Button click
Undo         undo|]


[rule|

data Command
ReplaceWith  replace Phrase with Phrase
Click        Times Button click
Undo         undo

|]


-- $ cabal build && cabal exec runhaskell sources/Example/Commands.hs
-- $ cabal exec -- ghc  -outputdir ignore/ignore  -ddump-splices  sources/Example/Commands.hs
main :: IO ()
main = do

 putStrLn ""
 print grammar

 putStrLn ""
 print $ ReplaceWith (Words ["this", "and", "that"]) (Words ["that", "and", "this"])

