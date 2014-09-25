#include <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>

#include "events.h"


ProcessSerialNumber currentApplication()
{
  // need the Carbon PSN
  // launchedApplications is deprecated
  NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];
  // NSLog(@"%@", appInfo);

  ProcessSerialNumber psn;
  psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
  psn.lowLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"] unsignedIntValue];

  return psn;
}

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

void pressKey(CGKeyCode key, CGEventFlags command, CGEventFlags control, CGEventFlags shift, CGEventFlags option, CGEventFlags function)
{
    // events to press a key
    CGEventRef event1 = CGEventCreateKeyboardEvent(NULL, key, true); // key down
    CGEventRef event2 = CGEventCreateKeyboardEvent(NULL, key, false); // key up

    // add modifiers ('command-shift-key') to event
    CGEventSetFlags(event1, command | control | shift | option | function);
    CGEventSetFlags(event2, command | control | shift | option | function);

    ProcessSerialNumber psn = currentApplication();

    // send keyboard event to application process (a quartz event)
    CGEventPostToPSN(&psn, event1);
    CGEventPostToPSN(&psn, event2);

}

CGPoint currentMousePosition()
{
  CGEventRef event = CGEventCreate(NULL);
  CGPoint point = CGEventGetLocation(event);
  // NSLog(@"x= %f, y = %f", (float)mousePosition.x, (float)mousePosition.y);

  return point;
}

void clickMouse(CGEventType mouseDown, CGEventType mouseUp, CGMouseButton mouseButton, UInt32 clickCount, CGEventFlags command)
{

  CGPoint mousePosition = currentMousePosition();

  CGEventRef event1 = CGEventCreateMouseEvent(NULL, mouseDown, mousePosition, mouseButton);
  CGEventRef event2 = CGEventCreateMouseEvent(NULL, mouseUp,   mousePosition, mouseButton);

  // (necessary, but isn't it already set in constructor?)
  CGEventSetType(event1, mouseDown);
  CGEventSetType(event2, mouseUp);

  // hold down modifier key while clicking
  CGEventSetFlags(event1, command);
  CGEventSetFlags(event2, command);

  // for double-click
  // flaky: maybe better to repeat `CGEventPost`
  CGEventSetIntegerValueField(event1, kCGMouseEventClickState, clickCount);
  CGEventSetIntegerValueField(event2, kCGMouseEventClickState, clickCount);

  // kCGHIDEventTap "specifies that an event tap is placed at the point where HID system events enter the window server."
  CGEventPost(kCGHIDEventTap, event1);
  CGEventPost(kCGHIDEventTap, event2);

}


int main(int argc, char** argv)
{
  // Automatic Reference Counting (ARC)
  @autoreleasepool {

  // double-click the mouse while holding command, at current location
  clickMouse(kCGEventLeftMouseDown, kCGEventLeftMouseUp, kCGMouseButtonLeft, 2, kCGEventFlagMaskCommand);

  // type command-shift-n, into current application
  pressKey(kVK_ANSI_N, kCGEventFlagMaskCommand, 0, kCGEventFlagMaskShift, 0, 0);
  }

  return 0;
}
