#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>
#import "actor.h"


int main(int argc, char** argv)
{
  // Automatic Reference Counting (ARC)
  @autoreleasepool {

  // type command-shift-n, into current application
  [Actor pressKey:kVK_ANSI_B withModifiers:(kCGEventFlagMaskCommand | kCGEventFlagMaskControl)];

  [NSThread sleepForTimeInterval:(30 / 1000.0f)];

  // double-click the mouse while holding command, at current location
  [Actor clickMouse:kCGEventLeftMouseDown and:kCGEventLeftMouseUp on:kCGMouseButtonLeft for:1 with:0];
}

  return 0;
}
