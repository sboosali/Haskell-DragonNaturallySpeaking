PACKAGE = commands
VERSION = 0.0.0

HC         = cabal exec -- ghc
LIBDIR     = $(shell $(HC) --print-libdir)
CFLAGS     = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS    = -v 
PACKAGES   = -package template-haskell -package language-c-quote -package language-c-inline -package bv -package commands
FRAMEWORKS = -framework Carbon -framework Cocoa -framework Foundation
LDFLAGS    = -optl-ObjC $(PACKAGES) $(FRAMEWORKS)

# # # # # # # # # # # # # # # # # # 


Haskell: install run

install:
	find . | grep \.hi$ | grep commands # (debugging)
	cabal build # reinstalls commands package with "cabal install commands"

run: Main
	./Main

# "$@" is the output variable i.e. target file i.e. object file e.g. "Main"
# "$^" are the space-separated input variables i.e. prerequisites i.e. dependency files i.e. source files
Main: Events.o Events_objc.o Main.o
	$(HC) $(LDFLAGS)  -o $@  $^

# "%" is a wildcard
# "$<" is the first input variable i.e. prerequisite e.g. "%.hs"
Events.o: Events.hs dist/build
	$(HC) $(HCFLAGS)  -c $<

Events_objc.m: Events.o

# module "Main" must have package "main" 
# Main imports Events
Main.o: Main.hs Events.o
	$(HC) $(HCFLAGS)  -c $<


# # # # # # # # # # # # # # # # # # 

Objective-C: main
	./main

main: main.m actor.m
	gcc -ObjC -framework Cocoa  -o ./main  $^


# # # # # # # # # # # # # # # # # # 

default: Haskell

all:
	cabal build
	cabal test
	cabal haddock
	hlint .

clean:
	rm -f *.o *.hi *_objc.[hm] Main
	rm -f main

fresh: clean
	rm -fr dist

.PHONY: default all clean fresh install Haskell Objective-C

# # # # # # # # # # # # # # # # # # 

