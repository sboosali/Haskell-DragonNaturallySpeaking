#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>
#import "actor.h"


ProcessSerialNumber currentApplication() {
  NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];

/* // NSLog(@"%@", appInfo);

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

  // need the Carbon PSN
  ProcessSerialNumber psn;
  psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
  psn.lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];

  return psn;
}

CGPoint currentMousePosition()  {

  CGEventRef event = CGEventCreate(NULL);
  CGPoint point = CGEventGetLocation(event);
  // NSLog(@"x= %f, y = %f", (float)mousePosition.x, (float)mousePosition.y);

  return point;
}


@implementation Actor

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

    ProcessSerialNumber psn = currentApplication();

    // send keyboard event to application process (a quartz event)
    CGEventPostToPSN(&psn, event1);
    CGEventPostToPSN(&psn, event2);
}

+(void) clickMouse:(CGEventType)mouseDown and:(CGEventType)mouseUp on:(CGMouseButton)mouseButton for:(UInt32)clickCount with:(CGEventFlags)modifiers {

  CGPoint mousePosition = currentMousePosition();

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

@end
