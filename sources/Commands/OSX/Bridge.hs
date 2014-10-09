module Commands.OSX.Bridge where
import Commands.Etc
import Commands.Types
import Commands.Munging

import Data.List.Split (splitOn) 
import Safe
import Filesystem.Path.CurrentOS()
import Filesystem.Path
import Filesystem.Path.Rules

import Prelude hiding (FilePath)
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Char
import Data.String
import Data.Maybe


-- | makes a dynamic 'String' into a static 'Application', or a default dynamic 'Application'
fromNSApplicationPath :: String -> Application
fromNSApplicationPath path = maybe (ApplicationPath path) id (path2application path)

-- | maybe dynamically 'read's a path to an 'Application'
path2application :: String -> Maybe Application 
path2application = fromString >>> basename >>> encodeString darwin >>> toConstructor >=> readMay

-- | may make a string into a valid Haskell constructor
toConstructor :: String -> Maybe String
toConstructor = dropUntil isAlpha >>> splitOn " " >>> map (filter isAlphaNum) >>> classCase >>> list Nothing Just

-- | a Haskell constructor is a Haskell identifier that starts with an uppercase letter
-- 
-- <http://www.haskell.org/onlinereport/lexemes.html>
-- 
-- >>> all (const False) []
-- True
isConstructor :: String -> Bool
isConstructor [] = False
isConstructor (first:rest) = isUpper first && isIdentifier rest

-- | a Haskell identifier is made up of: a letter, followed by zero or more letters, digits, underscores, single quotaes
-- 
isIdentifier :: String -> Bool
isIdentifier = all ((||) <$> isAlphaNum <*> (`elem` "_'"))

