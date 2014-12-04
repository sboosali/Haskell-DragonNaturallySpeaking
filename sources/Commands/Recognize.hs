{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
module Commands.Recognize where
import Commands.Etc

import Text.InterpolatedString.Perl6

import Data.Tree
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Category ((>>>))
import Language.Haskell.TH


-- | crappy lawless adhoc typeclass.
--
-- @a@ is the "recognizable" type, @b@ is the "recognition format".
--
-- 'recognize' takes a proxy value.
--
-- a serialized value is obviously finite. we can serialize infinite
-- (i.e. cyclic) data structures with indirect references. a cyclic
-- data structure can be represented with direct or indirect
-- references.
--
-- Here, the output 'Tree' is really a graph, as its 'Node's may be
-- mutually-recursive. we represent these cycles with direct
-- references, exploiting Haskell's non-strictness.
--
-- why this mess? I want the user to be able to override the @instance@
-- of a "child" type, and given only a reference to the @instance@ of
-- some "parent" type, we can get the whole grammar, all the way "down".
-- (I doubt this is the best way to do it).
--
class Recognize b a where
 recognize :: a -> Tree b

-- | a 'NatLink' rule has a left-hand side and a right-hand side.
--
data NatLink = NatLink Name String
 deriving (Show,Eq,Ord)


-- | outputs a valid (Python string) @natlink.GrammarBase.gramSpec@, in NatLink's BNF-like format.
-- see @<./NatLink/MacroSystem/core/gramparser.py>@ once you get the NatLink source.
--
-- the NatLink grammar specification is included verbatim below:
--
-- >   Rule Definition:
-- >       <RuleName> imported ;
-- >       <RuleName> = Expression ;
-- >       <RuleName> exported = Expression ;
-- >
-- >   A rule needs the keyword "exported" in order to be activated or visible
-- >   to other grammars for importing.
-- >
-- >   Expression:
-- >       <RuleName>                  // no spaces
-- >       {ListName}                  // no spaces
-- >       Word
-- >       "Word"                      // with spaces
-- >       ( Expression )
-- >       [ Expression ]              // optional
-- >       Expression +                // repeat
-- >       Expression Expression       // sequence
-- >       Expression | Expression     // alternative
--
--
-- import all builtin NatLink rules, in case a user wants to use them.
-- (e.g. without having to hack a 'Recognize' instance by putting
-- new lines in the right-hand side String etc.)
--
-- serializes a directly recursive graph representation of a
-- 'NatLink' grammar into an "indirectly recursive" 'String'.
--
--
-- given the input:
--
-- > Node (NatLink ''Command "<Commands__Spiros__Phrase> | ...;")
-- >      [recognize (undefined :: Commands.Rule.Phrase), ...]
--
-- and the 'Recognize' instance:
--
-- > instance Recognize NatLink Phrase where
-- >  recognize _ = Node (NatLink ''Phrase "<dgndictation>") []
--
-- it serializes it into:
--
-- > "
-- > <dgndictation> imported;
-- >  ... imported;
-- > <Command> exported = <Phrase> | ...;
-- > <Phrase> = <dgndictation>;
-- > "
--
-- only the 'rootLabel' is @"exported"@.
--
-- the left-hand sides are unique, given alphabetic identifiers. which
-- 'Commands.TH.Syntax.pCid' returns, by construction.
-- (an implicit dependency)
--
serializeNatLinkGrammar :: Tree NatLink -> String
serializeNatLinkGrammar graph = intercalate "\n" (preludeNatLink ++ serialize nodes)
 where
 nodes = searchGraph graph
 serialize []             = []  -- TODO
 serialize (label:labels) = serializeNatLinkExport label : map serializeNatLinkRule labels

-- | all builtin NatLink rules that can be @imported@, afaik. namely:
--
-- * @\<dgndictation\>@ = repeated dictation words
-- * @\<dgnletters\>@   = repeated spelling letters
-- * @\<dgnwords\>@     = set of all dictation words
--
preludeNatLink :: [String]
preludeNatLink = ["<dgndictation> imported;", "<dgnletters> imported;", "<dgnwords> imported;"]

-- |
--
serializeNatLinkExport :: NatLink -> String
serializeNatLinkExport (NatLink name body) = [qq|{serializeNatLinkName name} exported = {body};|]

-- |
--
serializeNatLinkRule :: NatLink -> String
serializeNatLinkRule (NatLink name body) = [qq|{serializeNatLinkName name} = {body};|]

-- |
--
-- input:
--
-- > ''Phrase
--
-- output:
--
-- > "<Phrase>"
--
serializeNatLinkName :: Name -> String
serializeNatLinkName name = [qq|<{nameBase name}>|]

