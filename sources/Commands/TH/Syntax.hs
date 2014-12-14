{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | 
--
-- defines the syntax tree and template parsers the "Commands.TH" module hierarchy uses.
-- 
module Commands.TH.Syntax where
import Commands.Text.Parsec
import Commands.Grammar

import Data.List.NonEmpty (NonEmpty(..))

import Prelude hiding (head)
import Control.Applicative hiding (many,(<|>))
import Language.Haskell.TH.Syntax


-- | wraps 'pProductions'
pGrammar :: Parser Char Grammar
pGrammar = Grammar <$> pProductions

-- | we need the 'try', because 'pProductions' consumes 'newline's
--
pProductions :: Parser Char (NonEmpty Production)
pProductions = between whitespace (whitespace <* eof) $ pProduction `sepBy1Slow` whitespace

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
-- (pretty-printing 'NonEmpty' as @[]@, and eliding the @newtype@ constructors):
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
-- > Variant ''ReplaceWith (Part "replace" :| [Hole (NonTerminal ''Phrase), Part (Terminal "with"), Hole (NonTerminal ''Phrase)])
--
--
pVariant :: Parser Char Variant
pVariant = Variant
 <$> (mkName <$> pCid)
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
pTerminal :: Parser Char Terminal
pTerminal = Terminal <$> pVid

-- | 
-- e.g. lifted @(\'P\':"hrase")@
pNonTerminal :: Parser Char NonTerminal
pNonTerminal = (NonTerminal . mkName) <$> pCid

-- | i.e. @V@ariable @id@entifier
-- 
-- the alphabetic subset of valid Haskell type-level identifiers (no "_" or "'")
--
pVid :: Parser Char String
pVid = spaced ((:)
 <$> lower
 <*> many letter)

-- | i.e. @C@onstructor @id@entifier
--
-- the alphabetic subset of valid Haskell value-level identifiers (no "_" or "'")
-- 
pCid :: Parser Char String
pCid = spaced ((:)
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

