{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
-- |
--
-- 'buildRecognizeNatLinkI' needs these 'Name's in templates:
--
-- * import Commands.Recognize (Recognize, recognize, NatLink)
-- * import Data.Tree (Tree, Node) 
--
module Commands.TH.Instance.Recognize where
import Commands.Etc
import Commands.Recognize
import Commands.Grammar
import Commands.TH.Syntax

import Control.Lens
import Data.Data.Lens
import Data.List.NonEmpty (NonEmpty(..),toList)
import Data.List (intercalate)
import Language.Haskell.TH.Lift
import Text.InterpolatedString.Perl6

import Control.Applicative
import Control.Category ((>>>))
import Data.Tree
import Language.Haskell.TH


-- | 
--
-- given: 
--
-- > Production ''Command [
-- >  Variant ''ReplaceWith  [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase],
-- >  Variant ''Click        [Hole ''Times, Hole ''Button, Part "click"],
-- >  Variant ''Undo         [Part "undo"]
-- >  ]
-- 
-- it builds a 'Recognize' @instance@ like:
--
-- > instance Recognize NatLink Command where
-- >  recognize _ = Node (NatLink
-- >                      ''Command
-- >                      "replace <Phrase> with <Phrase> | <Times> <Button> click | undo")
-- >                     [ recognize (undefined :: Phrase) :: Tree NatLink
-- >                     , recognize (undefined :: Times)  :: Tree NatLink
-- >                     , recognize (undefined :: Button) :: Tree NatLink
-- >                     ]
--
-- (eliding the module qualifications for readability)
-- 
buildRecognizeNatLinkI :: Production -> Q [Dec]
buildRecognizeNatLinkI (Production (NonTerminal name) variants) = do

 let typ = pure (ConT name)
 let pat = [p| _ |]
 let exp = [e| Node (NatLink $(nameE) $(bodyE)) $(childrenE) |]

 [d| instance Recognize NatLink $(typ) where recognize $(pat) = $(exp) |]

 where
 nameE     = lift name
 bodyE     = lift (buildNatLinkBody (map (^. (symbols.to toList)) (toList variants))) -- TODO fix this crap
 childrenE = ListE <$> mapM buildRecognizeNatLinkCall names
 names     = uniques (map unNonTerminal (variants^..biplate))

-- | 
--
--
-- input: 
--
-- >  [ [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase]
-- >  , [Hole ''Times, Hole ''Button, Part "click"]
-- >  , [Part "undo"]
-- >  ]
--
-- output:
--
-- > "replace <Phrase> with <Phrase> | <Times> <Button> click | undo"
--
buildNatLinkBody :: [[Symbol]] -> String
buildNatLinkBody = map buildNatLinkVariant >>> intercalate " | "

-- | 
--
--
-- input: 
--
-- > [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase]
--
-- output:
--
-- > "replace <Phrase> with <Phrase>"
--
--
buildNatLinkVariant :: [Symbol] -> String
buildNatLinkVariant = map buildNatLinkSymbol >>> intercalate " "

-- | 
--
-- e.g.
--
-- >>> buildNatLinkSymbol (Part "with")
-- "with"
-- >>> buildNatLinkSymbol (Hole ''Phrase)
-- "<Module_SubModule_Phrase>"
--
buildNatLinkSymbol :: Symbol -> String
buildNatLinkSymbol (Part (Terminal token))   = token
buildNatLinkSymbol (Hole (NonTerminal name)) = serializeNatLinkName name

-- | 
--
-- > recognize (undefined :: Phrase) :: Tree NatLink
--
buildRecognizeNatLinkCall :: Name  -> Q Exp
buildRecognizeNatLinkCall name  = [e| recognize (undefined :: $(typ)) :: Tree NatLink |]
 where 
 typ = pure (ConT name)

