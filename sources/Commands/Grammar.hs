{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Commands.Grammar where
import Commands.Etc
import Commands.Instances() 

import Control.Lens
import Data.Data.Lens
import Data.List.NonEmpty (NonEmpty(..),head)
import Language.Haskell.TH.Lift

import Prelude hiding (head)
import Data.Typeable
import Data.Data
import Language.Haskell.TH (Name)


-- | 
class Rule a where
 grammar :: a -> Grammar


-- | see <http://en.wikipedia.org/wiki/Formal_grammar#Formal_definition>
--
-- these lenses extract a 'Grammar's "derived/implicit fields" from its 'Production's:
--
-- * 'terminals'
-- * 'nonTerminals'
-- * 'root'
-- 
newtype Grammar = Grammar { _productions :: NonEmpty Production }
 deriving (Show,Eq, Data,Typeable)

-- |
data Production = Production
 { _lhs :: NonTerminal -- ^ type name
 , _rhs :: NonEmpty Variant
 -- TODO parametric non-terminals / higher-kinded types
 }
 deriving (Show,Eq, Data,Typeable)

-- | "label" as in Labeled BNF (LBNF). see <http://hackage.haskell.org/package/BNFC-meta>.
data Variant = Variant
 { _label   :: Name -- ^ constructor name
 , _symbols :: NonEmpty Symbol
 }
 deriving (Show,Eq, Data,Typeable)

-- | an 'rhs' is some 'Terminal's and 'NonTerminal's
data Symbol
 = Part Terminal
 | Hole NonTerminal
 deriving (Show,Eq, Data,Typeable)

-- |
newtype Terminal = Terminal { unTerminal :: String }
 deriving (Show,Eq,Ord, Data,Typeable)

-- |
newtype NonTerminal = NonTerminal { unNonTerminal :: Name }
 deriving (Show,Eq,Ord, Data,Typeable)

-- splice must follow declarations
$(concatMapM makeLenses [''Grammar, ''Production, ''Variant])

-- 'Char' and @[a]@ and 'Name' have 'Lift' instances
$(concatMapM deriveLift [''Grammar, ''Production, ''Variant, ''Symbol, ''Terminal, ''NonTerminal])

-- | exact because 'Terminal' is a @newtype@ not a @type@ alias
terminals :: Traversal' Grammar Terminal
terminals = biplate

-- |  exact because 'NonTerminal' is a @newtype@ not a @type@ alias
nonTerminals :: Traversal' Grammar NonTerminal
nonTerminals = biplate

start :: Getter Grammar NonTerminal
start = productions . to head . lhs

-- | a 'Traversal', not a 'Lens', because there are multiple targets.
grammar'symbols :: Traversal' Grammar Symbol
grammar'symbols = productions.each.rhs.each.symbols.each


getStart :: Grammar -> Name
getStart = unNonTerminal . view start

getParts :: Grammar -> [String]
getParts = uniques . map unTerminal . toListOf terminals

getHoles :: Grammar -> [Name]
getHoles = uniques . map unNonTerminal . toListOf nonTerminals

