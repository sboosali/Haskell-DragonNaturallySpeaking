#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>


@interface Actor : NSObject {}
+(NSString*) currentApplicationPath;
+(void) pressKey:(CGKeyCode)key withModifiers:(CGEventFlags)modifiers;
+(void) clickMouse:(CGEventType)mouseDown and:(CGEventType)mouseUp on:(CGMouseButton)mouseButton for:(UInt32)clickCount with:(CGEventFlags)modifiers;
@end
