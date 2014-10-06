PACKAGE = commands
VERSION = 0.0.0

HC         = cabal exec -- ghc
LIBDIR     = $(shell $(HC) --print-libdir)
CFLAGS     = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS    = -v -idist/build/ -package-name $(PACKAGE)-$(VERSION)
PACKAGES   = -package template-haskell -package language-c-quote -package language-c-inline -package commands
FRAMEWORKS = -framework Carbon -framework Cocoa -framework Foundation
LDFLAGS    = $(PACKAGES) $(FRAMEWORKS) 


# # # # # # # # # # # # # # # # # # 

default: Haskell

clean:
	rm -f *.o *.hi *_objc.[hm] Main
	rm -fr dist
	rm -f events

.PHONY: default clean Haskell Objective-C

# # # # # # # # # # # # # # # # # # 

Haskell: Main
	./Main

# "$@" is the output variable i.e. target file i.e. object file e.g. "Main"
# "$^" are the space-separated input variables i.e. prerequisites i.e. dependency files i.e. source files
Main: Main.o Events_objc.o Events.o
	$(HC) $(LDFLAGS)  -o $@  $^

# Main imports Events
Main.o: Events.o

Events_objc.m: Events.o

# "%" is a wildcard
# "$<" is the first input variable i.e. prerequisite e.g. "%.hs"
%.o: %.hs dist/build
	$(HC) $(HCFLAGS)  -c $<

dist/build:
	cabal build


# # # # # # # # # # # # # # # # # # 

Objective-C: events
	./events

events: events.m
	gcc  events.m  -o ./events  -ObjC -framework Cocoa

# # # # # # # # # # # # # # # # # # 
