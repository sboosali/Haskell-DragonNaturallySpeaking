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


-- | serializes a directly recursive graph representation of a
-- 'NatLink' grammar into an "indirectly recursive" 'String'.
--
-- given the input:
--
-- > Node (NatLink ''Command "<Commands__Spiros__Phrase> | ...;") [recognize (undefined :: Commands.Rule.Phrase), ...]
--
-- and the 'Recognize' instance:
--
-- > instance Recognize NatLink Phrase where
-- >  recognize _ = Node (NatLink ''Phrase "<dgndictation>") []
--
-- it serializes it into:
--
-- > "
-- > <Main_Command> exported = <Commands_Spiros_Phrase> | ...;
-- > <Commands_Spiros_Phrase> = <dgndictation>;
-- > "
--
-- only the 'rootLabel' is @"exported"@.
--
-- the left-hand sides are unique, unless you violate Haskell style
-- in naming your modules/types. i.e. do not put "_" in in a module
-- such that it conflicts, and don't camel case variables such that
-- they conflict, with other modules, when reduced with
-- 'nameQualified_'.
--
-- e.g. @A_B.C@ and @A.B_C@ and @A.B.C@. all perversely reducing to
-- @"A_B_C"@.
--
-- implicit dependency: see "Commands.TH.Syntax.pCid". which returns
-- alphabetic-only identifiers, by construction.
--
serializeNatLinkGrammar :: Tree NatLink -> String
serializeNatLinkGrammar graph = intercalate "\n" $ serialize $ searchGraph graph
 where
 serialize []             = []  -- TODO
 serialize (label:labels) = serializeNatLinkExport label : map serializeNatLinkRule labels

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

