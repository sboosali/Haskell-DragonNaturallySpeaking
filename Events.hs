{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ViewPatterns #-}
module Events where
import Commands.Types
import Commands.OSX.Types
import Commands.OSX.Constants

import Data.BitVector
import Foreign.C.Types
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import [ "<Carbon/Carbon.h>" 
            , "<Cocoa/Cocoa.h>"
            , "<Foundation/Foundation.h>"]


-- |
-- Haskell String ~ Objective-C NSString
currentApplicationPathO :: IO String
currentApplicationPathO = $(objc [] $ [t|String|] <: [cexp|
 [[[NSWorkspace sharedWorkspace] activeApplication] objectForKey:@"NSApplicationPath"]
|])

-- | (KeyPress (code -> char) (encode -> flags)) 
pressO :: [Modifier] -> VirtualKey -> IO ()
pressO (mask -> flags) (code -> key) = $(objc ['flags :> [t|CULLong|], 'key :> [t|CUShort|]] $ void [cexp|
 NSLog(@"%qu %hu", flags, key)
|])

objc_emit


main =  do
 objc_initialise
 currentApplicationPathO >>= print 
 pressO [Command, Shift] KKey
