{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Commands.TH where
import Commands.Etc
import Commands.Text.Parsec

import Control.Lens
import Data.Data.Lens
import Data.Either.Utils
import Data.List.NonEmpty ( NonEmpty(..),toList,  head,tail,last,init )
import qualified Data.List.NonEmpty as NonEmpty

import Prelude (Show,Eq,Char,String,($),(.))
import Control.Monad
import Control.Applicative hiding (many,(<|>))
import Data.Either
import Data.Functor
import Data.Foldable (foldl,foldr,foldl',foldr',foldl1,foldr1)
import Data.Typeable
import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Quote


-- | see <http://en.wikipedia.org/wiki/Formal_grammar#Formal_definition>
data Grammar = Grammar
 { _terminals    :: [Terminal]
 , _nonTerminals :: [NonTerminal]
 , _productions  :: NonEmpty Production
 , _root         :: NonTerminal
 }
 deriving (Show,Eq,Data,Typeable)

-- |
data Production = Production
 { _lhs :: NonTerminal -- ^ type name
 , _rhs :: NonEmpty Variant
 -- TODO parametric non-terminals / higher-kinded types
 }
 deriving (Show,Eq,Data,Typeable)

-- |
data Variant = Variant
 { _constructor :: NonTerminal -- ^ constructor name
 , _symbols     :: NonEmpty Symbol
 }
 deriving (Show,Eq,Data,Typeable)

-- |
data Symbol
 = Part Terminal
 | Hole NonTerminal
 deriving (Show,Eq,Data,Typeable)

-- |
type Terminal = String

-- |
type NonTerminal = Name

-- splice must follow declarations
$(concatMapM makeLenses [''Grammar, ''Production, ''Variant])


parseGrammar :: String -> Grammar
parseGrammar template = Grammar terminals nonTerminals productions start
 where
 terminals            = findTerminals $ toList productions
 nonTerminals         = findNonTerminals $ toList productions
 start                = productions ^. (to head . lhs)
 Right productions    = many1 pProduction `parsing` template

-- |
findNonTerminals :: [Production] -> [NonTerminal]
findNonTerminals = toListOf biplate

-- |
findTerminals :: [Production] -> [Terminal]
findTerminals = toListOf biplate

-- |
-- given the input template:
-- 
-- > [rule| data Command
-- > ReplaceWith  replace Phrase with Phrase
-- > Click        Times Button click
-- > Undo         undo
-- > |]
--
-- 'pProduction' (when run) parses it into:
--
-- > Production ''Command [
-- >  Variant ''ReplaceWith  [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase],
-- >  Variant ''Click        [Hole ''Times, Hole ''Button, Part "click"],
-- >  Variant ''Undo         [Part "undo"]]
-- 
-- (pretty-printing @('NonEmpty' a)@ as @[a]@):
-- 
pProduction :: Parser Char Production
pProduction = whitespaced $ Production
 <$> (word "data" *> pNonTerminal <* newline)
 <*> many1 (pVariant <* newline) -- TODO sepEndBy1

-- |
-- given the input labeled-production:
-- 
-- > "ReplaceWith  replace Phrase with Phrase"
-- 
-- 'pVariant' (when run) parses it into:
--
-- > Variant ''ReplaceWith (Part "replace" :| [Hole ''Phrase, Part "with", Hole ''Phrase])
--
-- (bootstrapable?)
--
pVariant :: Parser Char Variant
pVariant = Variant
 <$> pNonTerminal
 <*> many1 pSymbol

-- | 
-- 
-- (bootstrapable?)
-- 
pSymbol :: Parser Char Symbol
pSymbol = try (Part <$> pTerminal)
      <|> try (Hole <$> pNonTerminal)
      <?> "alphanumeric Haskell identifier"

-- | 
-- e.g. lifted @('w':"ith")@
--
-- the alphabetic subset of valid Haskell type-level identifiers (no "_" or "'")
pTerminal :: Parser Char Terminal
pTerminal = spaced $ (:)
 <$> lower
 <*> many letter

-- | 
-- e.g. lifted @mkName ('P':"hrase")@
--
-- the alphabetic subset of valid Haskell value-level identifiers (no "_" or "'")
pNonTerminal :: Parser Char NonTerminal
pNonTerminal = mkName <$> spaced ((:)
 <$> upper
 <*> many letter)


