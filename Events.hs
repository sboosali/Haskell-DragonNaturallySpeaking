{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ViewPatterns #-}
module Events where
import Commands
import Commands.Types
import Commands.OSX.Marshall

import Data.BitVector
import Foreign.C.Types
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import [ "<Carbon/Carbon.h>" 
            , "<Cocoa/Cocoa.h>"
            , "<Foundation/Foundation.h>"]


objc_interface [cunit|@interface Actor : NSObject {}

+(NSString*)currentApplicationPath;

+(void) pressKey:(CGKeyCode)key withModifiers:(CGEventFlags)modifiers;

+(void) clickMouse:(CGEventType)mouseDown and:(CGEventType)mouseUp on:(CGMouseButton) mouseButton for:(UInt32) clickCount with:(CGEventFlags)modifiers;

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

+(NSString*) currentApplicationPath {
  return [[[NSWorkspace sharedWorkspace] activeApplication] objectForKey:@"NSApplicationPath"];
}

+(void) pressKey:(CGKeyCode)key withModifiers:(CGEventFlags)modifiers {
    // events to press a key
    CGEventRef event1 = CGEventCreateKeyboardEvent(NULL, key, true); // key down
    CGEventRef event2 = CGEventCreateKeyboardEvent(NULL, key, false); // key up

    // add modifiers ('command-shift-key') to event
    CGEventSetFlags(event1, modifiers);
    CGEventSetFlags(event2, modifiers);

    // current application
    NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];
    ProcessSerialNumber psn;
    psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
    psn.lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];

    // send keyboard event to application process (a quartz event)
    CGEventPostToPSN(&psn, event1);
    CGEventPostToPSN(&psn, event2);
}

+(void) clickMouse:(CGEventType)mouseDown and:(CGEventType)mouseUp on:(CGMouseButton)mouseButton for:(UInt32)clickCount with:(CGEventFlags)modifiers {

  // current mouse position
  CGPoint mousePosition = CGEventGetLocation(CGEventCreate(NULL));
  // NSLog(@"x= %f, y = %f", (float)mousePosition.x, (float)mousePosition.y);

  CGEventRef event1 = CGEventCreateMouseEvent(NULL, mouseDown, mousePosition, mouseButton);
  CGEventRef event2 = CGEventCreateMouseEvent(NULL, mouseUp,   mousePosition, mouseButton);

  // (necessary, but isn't it already set in constructor?)
  CGEventSetType(event1, mouseDown);
  CGEventSetType(event2, mouseUp);

  // hold down modifier key while clicking
  CGEventSetFlags(event1, modifiers);
  CGEventSetFlags(event2, modifiers);

  // for double-click
  // flaky: maybe better to repeat `CGEventPost`
  CGEventSetIntegerValueField(event1, kCGMouseEventClickState, clickCount);
  CGEventSetIntegerValueField(event2, kCGMouseEventClickState, clickCount);

  // kCGHIDEventTap "specifies that an event tap is placed at the point where HID system events enter the window server."
  CGEventPost(kCGHIDEventTap, event1);
  CGEventPost(kCGHIDEventTap, event2);
}

@end|]


-- |
-- Haskell String ~ Objective-C NSString
currentApplicationPathO :: IO String
currentApplicationPathO = $(objc [] $ [t|String|] <: [cexp|
 [[[NSWorkspace sharedWorkspace] activeApplication] objectForKey:@"NSApplicationPath"]
|])

pressO :: KeyPress -> IO ()
pressO (Press (encodeModifiers -> flags) (encodeKey -> key)) = $(objc ['flags :> [t|CULLong|], 'key :> [t|CUShort|]] $ void [cexp|
  [Actor pressKey:kVK_ANSI_B withModifiers:(kCGEventFlagMaskCommand | kCGEventFlagMaskControl)]
|])


objc_emit

main =  do
 objc_initialise
 currentApplicationPathO >>= print 
 pressO $ touch 'K'
