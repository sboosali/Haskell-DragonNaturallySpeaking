{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ViewPatterns #-}
module Main where

import Data.Typeable
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import [ "<Carbon/Carbon.h>" 
            , "<Cocoa/Cocoa.h>"]


currentApplicationPath :: IO String
currentApplicationPath = $(objc [] $ [t|String|] <: [cexp|
 [[[NSWorkspace sharedWorkspace] activeApplication] objectForKey:@"NSApplicationPath"]
|])

objc_emit


main = do
 objc_initialise
 currentApplicationPath >>= print 

