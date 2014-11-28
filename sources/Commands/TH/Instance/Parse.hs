{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
-- |
--
-- 'buildParseI' needs these 'Name's in templates:
--
-- * import "Commands.Text.Parsec"  ('Parser','word')
-- * import "Commands.Parse"        ('Parse','parse','contextual')
-- * import "Commands.Generic"      ('Default','def')
-- * import "Prelude"               ('Show','Eq')  
--
module Commands.TH.Instance.Parse where
import Commands.Etc
import Commands.TH.Syntax
import Commands.Text.Parsec
import Commands.Parse
import Commands.Generic
import Commands.Grammar

import Data.List.NonEmpty (NonEmpty(..),toList)
import qualified Data.List.NonEmpty     as NonEmpty
import qualified Control.Monad.NonEmpty as NonEmpty

import Prelude (Show,show,Char,String,($),(.),map)
import Control.Monad
import Control.Applicative hiding (many,(<|>))
import Data.Maybe
import Data.Foldable (foldl,foldr,foldl1,foldr1)
import Language.Haskell.TH


-- |
data ConstructorSyntax = ConstructorSyntax Name [Terminal] [ArgumentSyntax]
 deriving (Show)

-- |
data ArgumentSyntax    = ArgumentSyntax    NonTerminal (Maybe Symbol) [Terminal]
 deriving (Show)


-- | 
-- given the input 'Production':
--
-- > Production ''Command [
-- >  Variant ''ReplaceWith  [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase],
-- >  Variant ''Click        [Hole ''Times, Hole ''Button, Part "click"],
-- >  Variant ''Undo         [Part "undo"]]
-- 
-- 'buildParseI' builds a 'Parse' @instance@ like:
--  
-- > instance Parse Command where
-- >  parse context
-- >   =    try ((pure ReplaceWith                                                <$ (word "replace"))
-- >                    <*> (parse (contextual (word "with")                   )  <* (word "with")   )
-- >                    <*> (parse (context                                    )                     ))
-- > 
-- >    <|> try ((pure Click                                                                         )
-- >                    <*> (parse (contextual (parse context :: Parser Button))                     )
-- >                    <*> (parse (contextual (word "click")                  )  <* (word "click")  ))
-- > 
-- >    <|> try (pure Undo                                                        <$ (word "undo")    )
-- 
-- I think this code could be pure (e.g. Exp), but must be impure (e.g. Q Exp), because the QuasiQuotes are impure.
-- the QuasiQuotes make the templates MUCH more readable.
--
-- the template has an implicit dependency on the generated class.
--
-- I try to use parentheses (e.g. @f (g x)@) over dollar (e.g. @f $ g x@), as @$@ is syntax for splicing.
--
-- I try to use @_@ over @'@ when naming related identifiers (e.g. @f@ and @f'@), as prefix apostrophes are syntax for making 'Name's. this contradicts @_@ meaning "ignore", like 'mapM_'.
--
buildParseI :: Production -> Q [Dec]
buildParseI (Production (NonTerminal lhs) rhs) = do

 let typ = pure (ConT lhs)
 let pat = pure (VarP contextN)
 let exp = buildTypeParser rhs

 [d| instance Parse $(typ) where parse $(pat) = $(exp) |]

 where

 -- the argument to the built function
 contextN = mkName "context"

 -- | makes a word parser
 -- e.g. @wordE "with"@ -> @word "with"@
 -- "wordS, wordL" i.e. "String, Literal"
 wordE :: String -> Q Exp
 wordE wordS = [e|  word  $wordL  |]
  where
  wordL = pure ((LitE . StringL) wordS)

 -- | @foldl operator@ mimics applying left-associative @operator@s
 buildTypeParser :: NonEmpty Variant -> Q Exp
 buildTypeParser rhs = do
  constructorSyntaxes <- NonEmpty.mapM chunkArguments         rhs
  constructorsE       <- NonEmpty.mapM buildConstructorParser constructorSyntaxes
  let typeE           =  foldl1 (|<|>|) constructorsE

  pure typeE

  where
  (|<|>|) = infixlE '(<|>)

 buildConstructorParser :: ConstructorSyntax -> Q Exp
 buildConstructorParser (ConstructorSyntax name (map unTerminal -> parts) arguments) = do

  nameE            <- [e| pure $(nameE_) |]              -- e.g. pure ReplaceWith
  partsE           <- mapM wordE parts                   -- e.g. [word "replace"]
  let constructorE =  foldl (|<*|) nameE partsE          -- e.g. (pure ReplaceWith <* word "replace")

  argumentsE       <- mapM buildArgumentParser arguments -- e.g. [(parse ... <* word "with")), (parse ...)]
  let parserE      =  foldl (|<*>|) constructorE argumentsE

  [e| try $(pure parserE) |]

  where

  (|<*|), (|<*>|) :: Exp -> Exp -> Exp
  (|<*|)  = infixlE '(<*)
  (|<*>|) = infixlE '(<*>)

  nameE_ = pure (ConE name)

 -- | 
 buildArgumentParser :: ArgumentSyntax -> Q Exp
 buildArgumentParser (ArgumentSyntax _ context (map unTerminal -> parts)) = do
  parserE       <- parserE_
  partsE        <- partsE_
  let argumentE =  foldl (|<*|) parserE partsE

  return argumentE

  where

  -- operator binding
  (|<*|) :: Exp -> Exp -> Exp
  (|<*|) = infixlE '(<*)

  parserE_ :: Q Exp
  parserE_ = [e| parse $(contextE context) |]

  -- | new context from old context, either:
  -- 
  -- * pass the old context on (i.e. @context@)
  -- * make a new word context (e.g. @contextual (word "click")@)
  -- * make a new full-parser context (e.g. @contextual (parse def :: Parser Char Number)@) 
  -- 
  contextE :: Maybe Symbol -> Q Exp
  contextE Nothing                          = pure (VarE contextN)
  contextE (Just (Part (Terminal token)))   = [e|  contextual $parserE  |] 
   where parserE = wordE token
  contextE (Just (Hole (NonTerminal name))) = [e|  contextual ( parse def :: $parserT )  |]
   where parserT = [t|  Parser Char $(pure (ConT name))  |]

  partsE_ = mapM wordE parts :: Q [Exp]


-- | 
--
-- by chunking initial 'Part's into the 'ConstructorSyntax', we subsume the U1 case (i.e. argument-less constructor).
-- by peeking at the next 'Symbol' with 'lookAhead', we can later build a 'ParsingContext' with the 'ArgumentSyntax'.
--
--
-- >>> chunkArguments (Variant 'ReplaceWith [Part "replace", Hole ''Phrase, Part "with", Hole ''Phrase])
-- ConstructorSyntax 'ReplaceWith ["replace"] [
--  ArgumentSyntax (''Phrase) (Just $ Part "with") ["with"],
--  ArgumentSyntax (''Phrase) Nothing []]
--
-- >>> chunkArguments (Variant 'Click [Hole ''Times, Hole ''Button, Part "click"])
-- ConstructorSyntax 'Click [] [
--  ArgumentSyntax (''Times) (Just $ Hole ''Button) []
--  ArgumentSyntax (''Button) (Just $ Part "click") ["click"]]
--
--
chunkArguments :: Variant -> Possibly ConstructorSyntax
chunkArguments (Variant name (toList -> symbols)) = constructor `parseThrow` symbols
 where

 constructor :: Parser Symbol ConstructorSyntax
 constructor = ConstructorSyntax name
  <$> many part
  <*> (argument `manyTill` eof)

 argument :: Parser Symbol ArgumentSyntax
 argument = ArgumentSyntax
  <$> hole
  <*> lookAhead (optionMaybe symbol)
  <*> many part

 part :: Parser Symbol Terminal
 part = tokenPrim show nextPosition testPart

 hole :: Parser Symbol NonTerminal
 hole = tokenPrim show nextPosition testHole

 symbol :: Parser Symbol Symbol
 symbol = anyToken

 testPart (Part x) = Just x
 testPart _ = Nothing

 testHole (Hole x) = Just x
 testHole _ = Nothing

 -- we increment the column by one, because each Symbol is a single token
 nextPosition position _ _ = incSourceColumn position 1

