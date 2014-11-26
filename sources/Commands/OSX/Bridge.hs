{-# LANGUAGE RankNTypes #-}
module Commands.OSX.Bridge where
import Commands.Etc
import Commands.Types
import Commands.Munging

import Data.List.Split (splitOn) 
import Filesystem.Path.CurrentOS()
import Filesystem.Path
import Filesystem.Path.Rules

import Prelude hiding (FilePath)
import Control.Arrow ((>>>))
import Control.Applicative
import Control.Monad
import Data.Char
import Data.String
import Data.Maybe


-- | makes a dynamic 'String' into a static 'Application', or a default dynamic 'Application'
fromNSApplicationPath :: String -> Application
fromNSApplicationPath path = fromMaybe Global (path2application path)

-- | maybe-dynamically 'read's a path to an 'Application'
--
-- prop> monadic failure
path2application :: String -> Possibly Application
path2application = fromString >>> basename >>> encodeString darwin >>> (toConstructor >=> readThrow)

-- | may make a string into a valid Haskell constructor
--
-- prop> monadic failure
toConstructor :: String -> Possibly String
toConstructor = filter isAscii >>> dropUntil isAlpha >>> splitOn " " >>> map (filter isAlphaNum) >>> classCase >>> list (failed "toConstructor") return

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

