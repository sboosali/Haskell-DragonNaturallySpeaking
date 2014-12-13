PACKAGE = commands
VERSION = 0.0.0

MODULES    = Example.hs Native.hs
OBJECTS    = Example.o Native.o
DEPENDS    = $(shell ./dependencies.sh commands criterion)

HC         = cabal exec -- ghc
LIBDIR     = $(shell $(HC) --print-libdir)
CFLAGS     = -v -fobjc-arc  -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS    = -v  -O2
PACKAGES   = -package commands -package template-haskell -package language-c-quote -package language-c-inline -package bv -package criterion -package deepseq-generics $(DEPENDS)
FRAMEWORKS = -framework Carbon -framework Cocoa -framework Foundation
LDFLAGS    = -optl-ObjC $(PACKAGES) $(FRAMEWORKS)

CODE = sources tests


# # # # # # # # # # # # # # # # # # 


Haskell: install run

install:
#	find . | grep \.hi$ | grep commands # (debugging)
	cabal build commands # reinstalls commands package with "cabal install commands"

run: Main
	./Main

# "$@" is the output variable i.e. target file i.e. object file e.g. "Main"
# "$^" are the space-separated input variables i.e. prerequisites i.e. dependency files i.e. source files
Main: Main.o Events.o Events_objc.o $(OBJECTS)
	$(HC) $(HCFLAGS) $(LDFLAGS)  -o $@  $^

# "%" is a wildcard
# "$<" is the first input variable i.e. prerequisite e.g. "%.hs"
Events.o: Events.hs dist/build $(OBJECTS)
	$(HC) $(HCFLAGS)  -c $<

Events_objc.m: Events.o

# module "Main" must have package "main" 
# Main imports Events
Main.o: Main.hs Events.o
	$(HC) $(HCFLAGS)  -c $<

# pure-Haskell ("native" versus "foreign") modules
$(OBJECTS): $(MODULES)
	$(HC) $(HCFLAGS)  --make $^



# # # # # # # # # # # # # # # # # # 

Objective-C: main
	./main

main: main.m actor.m
	gcc -ObjC -framework Cocoa  -o ./main  $^


# # # # # # # # # # # # # # # # # # 

all: test document check

build:
	cabal build

test: build
	cabal test
	cat dist/test/*-tests.log

document:
	cabal haddock
	open dist/doc/html/commands/index.html

check:
	hlint $(CODE)

# # # # # # # # # # # # # # # # # # 

default: Haskell

clean:
	rm -f Main *_objc.[hmo] {Main,Events}.{o,hi} *.{o,hi,dyn_o,dyn_hi} *.exe
	rm -f main

fresh: clean
	rm -fr dist

.PHONY: default all clean fresh install Haskell Objective-C

# # # # # # # # # # # # # # # # # # 
