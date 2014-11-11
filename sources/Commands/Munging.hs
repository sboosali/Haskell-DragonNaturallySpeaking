module Commands.Munging where



import Control.Arrow
import Data.Char
import Data.List


squeezeCase, dashCase, snakeCase :: [String] -> String
squeezeCase = intercalate ""
dashCase    = intercalate "-"
snakeCase   = intercalate "_"

classCase, camelCase :: [String] -> String
classCase = map capitalize >>> intercalate ""
camelCase [] = []
camelCase (word:words) = lower word ++ classCase words

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : lower xs

upper, lower :: String -> String
upper = map toUpper
lower = map toLower

