all: events
	./events

events: events.m events.h
	sh -c 'gcc  events.m  -o events  -ObjC -framework Cocoa'

clean:
	rm -f events
