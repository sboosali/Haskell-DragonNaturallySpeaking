{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Commands.Syntax where

import Data.Tuple.Utils
import Data.List.Utils hiding (split)
import Data.List.Split
import Data.List
import Data.Ord
import Data.Function

import Language.Haskell.TH
import Control.Arrow
import Control.Applicative


data Syntax = Part String | Hole Type | PartHoles String [Syntax]
 deriving (Show)

data Constructor = Constructor String [Type]
 deriving (Show)

constructors :: Info -> [Con]
constructors (TyConI (DataD    _ _ _ cs _)) = cs
constructors (TyConI (NewtypeD _ _ _ c  _)) = [c]

constructorType :: Con -> [Type]
constructorType (NormalC name (map snd -> types)) = types
constructorType (RecC name (map thd3 -> types)) = types

constructorName :: Con -> String
constructorName (NormalC name _) = nameBase name
constructorName (RecC name _) = nameBase name

syntaxT :: Info -> [[Syntax]]
syntaxT = map (parseC . fromCon) . constructors

fromCon :: Con -> Constructor
fromCon constructor = Constructor (constructorName constructor) (constructorType constructor)

parseC :: Constructor -> [Syntax]
parseC (Constructor ('X':name) types) = parseMixFix name types
parseC (Constructor name       []   ) = parseU1     name
parseC (Constructor _          types) = parseRaw         types

-- |
-- @instance Arrow (->)@ lets me think about e.g. @[(Index,Thing)]@ as:
-- a list of @Thing@ data with @Index@ metadata.
-- assert $ length typeHoles == length nameHoles
parseMixFix :: String -> [Type] -> [Syntax]
parseMixFix name types = map snd syntax
 where
 syntax = mergeBy (compare `on` fst) parts holes     -- ^ we merge on the Indices…
 parts = map (second Part) nameParts                 -- ^ after mapping over some Things…
 holes = map (second Hole) $ swpSnds types nameHoles -- ^ and swapping Things.
 (nameHoles, nameParts) = parseMixFixName name

parseMixFixName :: String ->  ([(Integer,String)], [(Integer,String)])
parseMixFixName name = partition (snd . second (=="_")) $ zip [0..] $ split (dropBlanks $ oneOf "_") name
 where dropBlanks = dropInitBlank . dropInnerBlanks . dropFinalBlank

-- | prop> length cs == length abs
swpSnds :: [c] -> [(a,b)] -> [(a,c)]
swpSnds cs abs = (unzip >>> second (const cs) >>> rezip) abs

-- | prop> rezip . unzip === id
rezip = uncurry zip

parseU1 :: String -> [Syntax]
parseU1 = (:[]) <$> Part

parseRaw :: [Type] -> [Syntax]
parseRaw = map Hole


type Phrase = String
data Command
 = Xreplace_with_ Phrase Phrase -- ^ by 'parseMixFix'
 | Undo                         -- ^ by 'parseU1'
 | Repeat Command               -- ^ by 'parseRaw'
 deriving (Show)

