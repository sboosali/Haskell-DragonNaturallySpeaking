all: events
	./events

events: events.m
	sh -c 'gcc  events.m  -o events  -ObjC -framework Cocoa'

clean:
	rm -f events
