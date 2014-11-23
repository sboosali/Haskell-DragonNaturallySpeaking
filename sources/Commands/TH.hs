{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Commands.TH where
import Commands.Etc
import Commands.Text.Parsec
import Commands.Parse

import Control.Lens
import Data.Data.Lens
import Data.Either.Utils
import Data.List.NonEmpty ( NonEmpty(..),toList,  head,tail,last,init )
import qualified Data.List.NonEmpty as NonEmpty

import Prelude (Show,show,Eq,Char,String,($),(.),undefined)
import Control.Monad
import Control.Applicative hiding (many,(<|>))
import Data.Maybe
import Data.Either
import Data.Functor
import Data.List hiding (foldl,foldl1,head)
import Data.Foldable (foldl,foldr,foldl',foldr',foldl1,foldr1)
import Data.Typeable
import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
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




-- |
-- only defined for 'Dec'laration contexts
rule = QuasiQuoter 
 { quoteExp   = undefined
 , quotePat   = undefined
 , quoteType  = undefined
 , quoteDec   = buildRule
 }

-- |
-- 
-- flow: @'String' -> 'Grammar' -> 'Q' ['Dec']@
-- 
buildRule :: String -> Q [Dec]
buildRule template = do
 return datatypes

 where
 datatypes = buildDataD <$> productions'

 productions' = toList (grammar^.productions)
 grammar = parseGrammar template




-- | unsafe
parseGrammar :: String -> Grammar
parseGrammar template = Grammar terminals nonTerminals productions start
 where
 terminals            = findTerminals $ toList productions
 nonTerminals         = findNonTerminals $ toList productions
 start                = productions ^. (to head . lhs)
 Right productions    = pGrammar `parsing` template

-- |
findNonTerminals :: [Production] -> [NonTerminal]
findNonTerminals = toListOf biplate

-- |
findTerminals :: [Production] -> [Terminal]
findTerminals = toListOf biplate




-- |
-- we need the 'try', because 'pGrammar' consumes 'newline's
pGrammar :: Parser Char (NonEmpty Production)
pGrammar = (between whitespace whitespace $ pProduction `sepBy1Slow` whitespace) <* eof

-- |
-- given the input template:
-- 
-- > [rule| data Command
-- > ReplaceWith  replace Phrase with Phrase
-- > Click        Times Button click
-- > Undo         undo |]
--
-- 'pProduction' parses it into:
--
-- > Production ''Command [
-- >  Variant ''ReplaceWith  [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase],
-- >  Variant ''Click        [Hole ''Times, Hole ''Button, Part "click"],
-- >  Variant ''Undo         [Part "undo"]]
-- 
-- (pretty-printing 'NonEmpty' as @[]@):
-- 
pProduction :: Parser Char Production
pProduction = Production
 <$> (word "data" *> pNonTerminal <* newline)
 <*> (pVariant `sepBy1Slow` newline)
 <?> "Production"

-- |
-- given the input labeled-production:
-- 
-- > "ReplaceWith  replace Phrase with Phrase"
-- 
-- 'pVariant' parses it into:
--
-- > Variant ''ReplaceWith (Part "replace" :| [Hole ''Phrase, Part "with", Hole ''Phrase])
--
--
pVariant :: Parser Char Variant
pVariant = Variant
 <$> pNonTerminal
 <*> many1 pSymbol
 <?> "Variant"

-- | 
-- 
-- an alphanumeric Haskell identifier
pSymbol :: Parser Char Symbol
pSymbol = try (Part <$> pTerminal)
      <|> try (Hole <$> pNonTerminal)
      <?> "Symbol (alphanumeric Haskell identifier)"

-- | 
-- e.g. lifted @(\'w\':"ith")@
--
-- the alphabetic subset of valid Haskell type-level identifiers (no "_" or "'")
pTerminal :: Parser Char Terminal
pTerminal = spaced $ (:)
 <$> lower
 <*> many letter

-- | 
-- e.g. lifted @mkName (\'P\':"hrase")@
--
-- the alphabetic subset of valid Haskell value-level identifiers (no "_" or "'")
pNonTerminal :: Parser Char NonTerminal
pNonTerminal = mkName <$> spaced ((:)
 <$> upper
 <*> many letter)




-- | 
-- given the input 'Production':
--
-- > Production ''Command [
-- >  Variant ''ReplaceWith  [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase],
-- >  Variant ''Click        [Hole ''Times, Hole ''Button, Part "click"],
-- >  Variant ''Undo         [Part "undo"]]
-- 
-- 'buildDataD' builds a @data@ declaration:
--  
-- > data Command
-- >  = ReplaceWith  Phrase Phrase
-- >  | Click        Times Button
-- >  | Undo
-- >  deriving (Show,Eq)
--
-- i.e. ignore 'Terminal's, keep 'NonTerminal's
--
buildDataD :: Production -> Dec
buildDataD (Production lhs (toList -> rhs)) = DataD context typename parameters constructors derived
 where
 context      = []
 typename     = lhs
 parameters   = []
 constructors = buildConstructorC <$> rhs
 derived      = [''Show, ''Eq]

-- | 
-- given the input 'Variant':
--
-- > Variant ''ReplaceWith  [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase],
-- 
-- 'buildConstructorC' builds the constructor "declaration":
--
-- > ReplaceWith Phrase Phrase
--
buildConstructorC :: Variant -> Con
buildConstructorC (Variant constructor (toList -> symbols)) = NormalC constructor arguments
 where
 arguments = concatMap buildArgument symbols

 buildArgument :: Symbol -> [StrictType]
 buildArgument (Part {})          = []
 buildArgument (Hole nonTerminal) = [(NotStrict, ConT nonTerminal)]

