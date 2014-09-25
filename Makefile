all: events
	./events

events: events.m events.h
	gcc  events.m  -o events  -ObjC -framework Cocoa

clean:
	rm -f events
