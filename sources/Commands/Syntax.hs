{-# LANGUAGE TemplateHaskell #-}
module Commands.Syntax where

import Data.List.Utils hiding (split)
import Data.List.Split
import Data.List
import Data.Function
import Data.Data.Lens

import Language.Haskell.TH
import Control.Arrow
import Control.Applicative
import Control.Lens


data Syntax = Part String | Hole Type | PartHoles String [Syntax]
 deriving (Show)

data Constructor = Constructor String [Type]
 deriving (Show)

-- | given a reified declaration (should be @data@), takes the constructors, and makes the grammar
syntaxT :: Info -> [[Syntax]]
syntaxT = map (parseC . fromCon) . constructors

constructors :: Info -> [Con]
constructors = toListOf biplate

constructorType :: Con -> [Type]
constructorType = toListOf biplate

constructorName :: Con -> String
constructorName = maybe "" nameBase . firstOf biplate

fromCon :: Con -> Constructor
fromCon constructor = Constructor (constructorName constructor) (constructorType constructor)

parseC :: Constructor -> [Syntax]
parseC (Constructor ('X':name) types) = parseMixFix name types
parseC (Constructor name       []   ) = parseU1     name
parseC (Constructor _          types) = parseRaw         types

-- |
-- @instance Arrow (->)@ lets me think about e.g. @[(Index,Thing)]@ as:
-- a list of @Thing@ data with @Index@ metadata.
--
-- prop> length typeHoles == length nameHoles
--
-- e.g. constructor: @Xreplace_with_ Phrase Phrase@
parseMixFix :: String -> [Type] -> [Syntax]
parseMixFix name types = map snd syntax
 where
 -- we merge by the "Indices"…
 syntax = mergeBy (compare `on` fst) parts holes
 -- after munging the "Things". 
 parts = map (second Part)                 nameParts
 holes = map (second Hole) $ swpSnds types nameHoles
 (nameHoles, nameParts) = parseMixFixName name

-- | split by "_", index each token, separate the separators
--
-- >>> parseMixFixName "replace_with_"
-- ([(1,"_"),(3,"_")], [(0,"replace"),(2,"with")])
--
parseMixFixName :: String ->  ([(Integer,String)], [(Integer,String)])
parseMixFixName name = partition (snd . second (=="_")) $ zip [0..] $ split (droppingBlanks $ oneOf "_") name
 where droppingBlanks = dropInitBlank . dropInnerBlanks . dropFinalBlank

-- | prop> arguments should share same length
swpSnds :: [c] -> [(a,b)] -> [(a,c)]
swpSnds cs = unzip >>> second (const cs) >>> rezip

-- | prop> rezip . unzip === id
rezip :: ([a], [b]) -> [(a, b)]
rezip = uncurry zip

-- | 
-- e.g. constructor: @Undo@
parseU1 :: String -> [Syntax]
parseU1 = (:[]) <$> Part

-- | 
-- e.g. constructor: @Repeat Command@
parseRaw :: [Type] -> [Syntax]
parseRaw = map Hole

-- | 
--
-- >>> chunkUntil (/='_') "_a_b" :: [String]
-- ["_a","_b"]
--
-- >>> chunkUntil (/='_') "a__bc" :: [String]
-- ["a","__b","c"]
chunkUntil :: (a -> Bool) -> [a] -> [[a]]
chunkUntil p = split (dropFinalBlank $ keepDelimsR $ whenElt p)

