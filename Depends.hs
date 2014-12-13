{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import qualified Data.Conduit          as C
import           Data.Conduit          (Conduit)
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Binary   as CB
import           Data.Conduit.Shell
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S8

import           Control.Category      ((>>>))
import qualified System.Environment    as System
import           Data.Monoid

-- $ ./depends.sh commands

main = do
 [package] <- System.getArgs
 (run . depends) package

-- | >>> $ cabal exec -- ghc-pkg field $PACKAGE depends
depends :: String -> Segment ()
depends package = cabal "exec" "--" "ghc-pkg" "field" package "depends" $| buffered strip
 where
 strip :: ByteString -> ByteString
 strip = S8.drop $ length ("depends: " :: String)


-- |
buffered :: (ByteString -> ByteString) -> Segment ()
buffered f = conduit (CB.lines =$= CL.map f =$= unlinesC)

-- | inverse to 'CB.lines'
unlinesC :: (Monad m) => Conduit ByteString m ByteString
unlinesC = awaitForever pad
 where
 pad :: (Monad m) => ByteString -> Conduit ByteString m ByteString
 pad bytes = yield $ bytes <> "\n"

