{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Commands.TH where
import Commands.Etc
import Commands.Text.Parsec (Parser,word)  -- ^ 'buildParseI' implicitly depends on these 'Name's
import Commands.Text.Parsec
import Commands.Parse (Parse,parse,contextual)  -- ^ 'buildParseI' implicitly depends on these 'Name's
import Commands.Parse
import Commands.Generic (Default,def)  -- ^ 'buildParseI' depends on implicitly these 'Name's

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

sequence'NonEmpty :: (Monad m) => NonEmpty (m a) -> m (NonEmpty a)
sequence'NonEmpty m = do
 let mx :| mx's = m
 x <- mx
 x's <- sequence mx's
 return (x :| x's)

mapM'NonEmpty :: (Monad m) => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
mapM'NonEmpty f = sequence'NonEmpty . NonEmpty.map f

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
 parseInstances <- concatMapM buildParseI productions'
 return (datatypes ++ parseInstances)

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
-- I try to use @_@ over @'@ in when naming related identifiers (e.g. @f@ and @f'@), as prefix apostrophes are syntax for making 'Name's.
--
buildParseI :: Production -> Q [Dec]
buildParseI (Production lhs rhs) = do

 let typ = return $ ConT lhs
 let pat = return $ VarP contextN
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
  wordL = return $ (LitE . StringL) wordS

 -- | @foldl operator@ mimics applying left-associative @operator@s
 buildTypeParser :: NonEmpty Variant -> Q Exp
 buildTypeParser rhs = do
  let constructorSyntaxes = NonEmpty.map chunkArguments rhs
  constructorsE       <- mapM'NonEmpty buildConstructorParser constructorSyntaxes
  let typeE           =  foldl1 (|<|>|) constructorsE

  return typeE

  where
  (|<|>|) = infixlE '(<|>)

 buildConstructorParser :: ConstructorSyntax -> Q Exp
 buildConstructorParser (ConstructorSyntax name parts arguments) = do

  nameE            <- [e| pure $(nameE_) |]              -- e.g. pure ReplaceWith
  partsE           <- mapM wordE parts                   -- e.g. [word "replace"]
  let constructorE =  foldl (|<*|) nameE partsE          -- e.g. (pure ReplaceWith <* word "replace")

  argumentsE       <- mapM buildArgumentParser arguments -- e.g. [(parse ... <* word "with")), (parse ...)]
  let parserE      =  foldl (|<*>|) constructorE argumentsE

  [e| try $(return parserE) |]

  where

  (|<*|), (|<*>|) :: Exp -> Exp -> Exp
  (|<*|)  = infixlE '(<*)
  (|<*>|) = infixlE '(<*>)

  nameE_ = return $ ConE name

 -- | 
 buildArgumentParser :: ArgumentSyntax -> Q Exp
 buildArgumentParser (ArgumentSyntax _ context parts) = do
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
  contextE Nothing                   = return $ VarE contextN
  contextE (Just (Part terminal))    = [e|  contextual $parserE  |] 
   where parserE                      = wordE terminal
  contextE (Just (Hole nonTerminal)) = [e|  contextual ( parse def :: $parserT )  |]
   where parserT                      = [t|  Parser Char $(return (ConT nonTerminal))  |]

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
chunkArguments :: Variant -> ConstructorSyntax
chunkArguments (Variant name (toList -> symbols)) = fromRight $ constructor `parsing` symbols
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

 part :: Parser Symbol String
 part = tokenPrim show nextPosition testPart

 hole :: Parser Symbol Name
 hole = tokenPrim show nextPosition testHole

 symbol :: Parser Symbol Symbol
 symbol = anyToken

 testPart (Part x) = Just x
 testPart _ = Nothing

 testHole (Hole x) = Just x
 testHole _ = Nothing

 nextPosition position _ _ = incSourceColumn position 1


-- | 
-- here and elsewhere in this module: 
-- * "meta" versions of functions have a "E" suffix (i.e. "Exp")
-- * "meta" versions of operators have a "|  |" circumfix (i.e. "[| |]" for templates)
--
-- mirroring the target code makes the template code more readable
--
-- the "meta" versions should obey (loosely...): 
-- @let (|+|) :: Exp -> Exp -> Exp;  x |+| y = UInfixE x (VarE '(+)) y@
-- @Language.Haskell.Meta.Parse.parseExp "x + y" == Right $ VarE 'x |+| VarE 'y@
--
-- or:
-- @let fE :: Exp -> Exp -> Exp;  fE x y = (VarE 'f) `AppE` x `AppE` y@
-- @Language.Haskell.Meta.Parse.parseExp "f x y" == Right $ fE (VarE 'x) (VarE 'y)@
--
-- this convention doesn't matter much, but I've been trying to think about how not to make my macro code illegible
--
infixlE :: Name -> Exp -> Exp -> Exp
infixlE name old new = InfixE (Just old) (VarE name) (Just new)


