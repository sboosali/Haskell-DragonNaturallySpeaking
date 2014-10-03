HC         = cabal exec -- ghc
LIBDIR     = $(shell $(HC) --print-libdir)
CFLAGS     = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS    = 
PACKAGES   = -package template-haskell -package language-c-quote -package language-c-inline
FRAMEWORKS = -framework Carbon -framework Cocoa
LDFLAGS    = $(PACKAGES) $(FRAMEWORKS) 

OBJS = Main.o Main_objc.o
NAME = Events


default: execute
clean:
	rm -f *.o *.hi Main_objc.[hm] $(NAME)
.PHONY: default clean


execute: $(NAME)
	./$(NAME)

$(NAME): $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

Main_objc.m: Main.o

Main.o:

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

