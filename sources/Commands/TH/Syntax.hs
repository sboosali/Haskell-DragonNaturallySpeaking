{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
-- | 
--
-- defines the syntax tree and template parsers the "Commands.TH" module hierarchy uses.
-- 
module Commands.TH.Syntax where
import Commands.Etc
import Commands.Text.Parsec

import Control.Lens
import Data.Data.Lens
import Data.List.NonEmpty (NonEmpty(..),toList,head)

import Prelude hiding (head)
import Control.Applicative hiding (many,(<|>))
import Data.Typeable
import Data.Data
import Language.Haskell.TH.Syntax


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
data ConstructorSyntax = ConstructorSyntax Name [Terminal] [ArgumentSyntax]
 deriving (Show)

-- |
data ArgumentSyntax    = ArgumentSyntax    NonTerminal (Maybe Symbol) [Terminal]
 deriving (Show)




-- |
parseGrammar :: String -> Possibly Grammar
parseGrammar template = do

 productions              <- parseThrow pGrammar template
 let terminals            =  findTerminals $ toList productions
 let nonTerminals         =  findNonTerminals $ toList productions
 let start                =  productions ^. (to head . lhs)

 return $ Grammar terminals nonTerminals productions start

-- |
findNonTerminals :: [Production] -> [NonTerminal]
findNonTerminals = toListOf biplate

-- |
findTerminals :: [Production] -> [Terminal]
findTerminals = toListOf biplate

-- |
-- we need the 'try', because 'pGrammar' consumes 'newline's
pGrammar :: Parser Char (NonEmpty Production)
pGrammar = between whitespace (whitespace <* eof) $ pProduction `sepBy1Slow` whitespace

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
-- here and elsewhere in this module hierarchy: 
--
-- * "syntactic" versions of functions have a \"E\" suffix (i.e. 'Exp')
-- * "syntactic" versions of operators have a @|  |@ circumfix (i.e. @[| |]@ for templates)
--
-- mirroring the target code makes the template code more readable for me.
--
-- the "syntactic" versions should obey (loosely...): 
--
-- @let (|+|) :: Exp -> Exp -> Exp;  x |+| y = UInfixE x (VarE '(+)) y@
-- @Language.Haskell.Meta.Parse.parseExp "x + y" == Right $ VarE 'x |+| VarE 'y@
--
-- or maybe:
--
-- @let fE :: Exp -> Exp -> Exp;  fE x y = (VarE 'f) `AppE` x `AppE` y@
-- @Language.Haskell.Meta.Parse.parseExp "f x y" == Right $ fE (VarE 'x) (VarE 'y)@
--
-- this convention doesn't matter much, and I haven't thought it through, but I've been trying to think about how not to make my macro code illegible.
--
infixlE :: Name -> Exp -> Exp -> Exp
infixlE name old new = InfixE (Just old) (VarE name) (Just new)

