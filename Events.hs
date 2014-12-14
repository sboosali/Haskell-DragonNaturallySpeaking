{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ViewPatterns #-}
module Events where
import Native
import Commands
import Commands.Types
import Commands.OSX.Marshall

import Data.BitVector

import Control.Monad hiding (void)
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import [ "<Carbon/Carbon.h>" 
            , "<Cocoa/Cocoa.h>"
            , "<Foundation/Foundation.h>"]


objc_interface [cunit|@interface Actor : NSObject {}

+(typename NSString*)currentApplicationPath;

+(void) pressKey:(typename CGKeyCode)key withModifiers:(typename CGEventFlags)modifiers;

@end|]


objc_implementation [] [cunit|

@implementation Actor

/*
  NSDictionary: {
  NSApplicationBundleIdentifier = "org.gnu.Emacs";
  NSApplicationName = Emacs;
  NSApplicationPath = "/Applications/Notes.app";
  NSApplicationProcessIdentifier = 40831;
  NSApplicationProcessSerialNumberHigh = 0;
  NSApplicationProcessSerialNumberLow = 4195328;
  NSWorkspaceApplicationKey = "<NSRunningApplication: 0x7fe3c0e08b30 (org.gnu.Emacs - 40831)>";
  }
*/

+(typename NSString*) currentApplicationPath {
  return [[[NSWorkspace sharedWorkspace] activeApplication] objectForKey:@"NSApplicationPath"];
}

+(void) pressKey:(typename CGKeyCode)key withModifiers:(typename CGEventFlags)modifiers {
    // events to press a key
    typename CGEventRef event1 = CGEventCreateKeyboardEvent(NULL, key, true); // key down
    typename CGEventRef event2 = CGEventCreateKeyboardEvent(NULL, key, false); // key up

    // add modifiers ('command-shift-key') to event
    CGEventSetFlags(event1, modifiers);
    CGEventSetFlags(event2, modifiers);

    // current application
    typename NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];
    typename ProcessSerialNumber psn;
    psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
    psn.lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];

    // send keyboard event to application process (a quartz event)
    CGEventPostToPSN(&psn, event1);
    CGEventPostToPSN(&psn, event2);
}

@end|]


-- |
-- Haskell String ~ Objective-C NSString
--
-- @activeApplication@ versus @frontmostApplication@ versus @currentApplication@
--
currentApplicationPathO :: IO String
currentApplicationPathO = $(objc [] $ [t|String|] <: [cexp|
 [[[NSWorkspace sharedWorkspace] activeApplication] objectForKey:@"NSApplicationPath"]
|])

-- |
-- Haskell CULLong ~ C unsigned long long
-- Haskell CUShort ~ C unsigned short
pressO :: KeyPress -> IO ()
pressO  (Press (encodeModifiers -> flags) (encodeKey -> key)) =
 $(objc ['flags :> [t|CULLong|], 'key :> [t|CUShort|]] $
 void [cexp|
  [Actor pressKey:key withModifiers:flags]
|])


insert :: String -> [KeyPress]
insert = map (Press [] . key)

key :: Char -> Key
key 'a' = AKey

-- | 1000 characters
thousand = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

markWord = do
 pressO $ Press [Option       ] LeftArrowKey
 pressO $ Press [Option, Shift] RightArrowKey

backWord = do
 pressO $ Press [Option] LeftArrowKey

forWord = do
 pressO $ Press [Option] RightArrowKey

delay = threadDelay $ 1000000


objc_emit

main =  do
 objc_initialise

 --  pressO $ Press [Command, Shift] AKey

 -- -- near no conscious latency
 -- replicateM_ 100 (currentApplicationPathO >>= print)

 -- -- near no conscious latency
 -- mapM_ pressO $ insert thousand

 -- -- keyboard shortcuts don't need lag between each KeyPress (hence
 -- -- 'replicateM_', without 'interleave $ delay 25000'). only
 -- -- interaction needs lag (e.g. a mini-buffer pop-up).
 -- -- tested in Chrome.
 -- replicateM_ 5 delay
 -- replicateM_ 10 forWord
 -- delay
 -- replicateM_ 10 backWord
 -- delay
 -- markWord

 -- about 750 µs
 benchmark "currentApplicationPathO" currentApplicationPathO

 -- about 1 ms
 -- benchmark "pressO" (pressO $ Press [] AKey)

