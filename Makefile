OUTPUT = bin

all: events
	$(OUTPUT)/events

events: events.m
	mkdir $(OUTPUT) || true 
	gcc  events.m  -o $(OUTPUT)/events  -ObjC -framework Cocoa

clean:
	rm -f $(OUTPUT)/events
