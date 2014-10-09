module Commands.Munging where

import Data.List.Split (splitOn) 

import Control.Arrow
import Data.Char
import Data.List


squeezeCase :: [String] -> String
squeezeCase = intercalate ""
dashCase    = intercalate "-"
snakeCase   = intercalate "_"

classCase = map capitalize >>> intercalate ""

camelCase [] = []
camelCase (word:words) = lower word ++ classCase words

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : lower xs

upper = map toUpper
lower = map toLower

