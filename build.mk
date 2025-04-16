here:=$(dir $(lastword $(MAKEFILE_LIST)))
WHIFFLE=$(here)
all: out

GC_BASE=$(here)whippet/
GC_EMBEDDER_H=$(WHIFFLE)runtime/whiffle-gc.h
GC_EMBEDDER_CPPFLAGS=$(WHIFFLE_CPPFLAGS)
include $(GC_BASE)embed.mk

WHIFFLE_CPPFLAGS=-I $(WHIFFLE)include
WHIFFLE_CFLAGS=-foptimize-sibling-calls

out.o: out.c
	$(GC_COMPILE) $(WHIFFLE_CPPFLAGS) $(WHIFFLE_CFLAGS) -c $<
out: out.o $(GC_OBJS)
	$(GC_LINK) $^ $(GC_LIBS)

clean: 
	$(GC_V)rm -f out out.o $(GC_OBJS)

# Clear some of the default rules.
.SUFFIXES:
.SECONDARY:
%.c:;
Makefile:;
