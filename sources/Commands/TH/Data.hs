{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Commands.TH.Data where
import Commands.Grammar

import Data.List.NonEmpty (toList)

import Data.Functor
import Language.Haskell.TH
import Language.Haskell.TH.Syntax



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
 context              = []
 NonTerminal typename = lhs
 parameters           = []
 constructors         = buildConstructorC <$> rhs
 derived              = [''Show, ''Eq]

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
 buildArgument (Part {})                 = []
 buildArgument (Hole (NonTerminal name)) = [(NotStrict, ConT name)]

