#include <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>

#include "events.h"


ProcessSerialNumber currentApplication()
{
  // need the Carbon PSN
  // launchedApplications is deprecated
  NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];

  ProcessSerialNumber psn;
  psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
  psn.lowLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"] unsignedIntValue];

  NSLog(@"%@", appInfo);

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
  @autoreleasepool {

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

    // must manually release objects from methods named with "Create" or "Copy"
    CFRelease(event1);
    CFRelease(event2);

  }
}

int main(int argc, char** argv)
{
  pressKey(kVK_ANSI_N, kCGEventFlagMaskCommand, 0, kCGEventFlagMaskShift, 0, 0);
  return 0;
}

