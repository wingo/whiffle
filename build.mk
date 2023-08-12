here:=$(dir $(lastword $(MAKEFILE_LIST)))
WHIPPET=$(here)whippet/
WHIFFLE=$(here)

include $(WHIPPET)embed.mk

WHIFFLE_CFLAGS=-I $(WHIFFLE)include
EMBEDDER_TO_GC_CFLAGS=$(WHIFFLE_CFLAGS) -include $(WHIFFLE)runtime/whiffle-gc.h

out.o: out.c
	$(GC_COMPILE) $(WHIFFLE_CFLAGS) $(GC_TO_EMBEDDER_CFLAGS) -c $<
out: out.o $(GC_OBJS)
	$(GC_LINK) $(GC_LIBS) $^

clean: 
	$(GC_V)rm -f out out.o $(GC_OBJS)

# Clear some of the default rules.
.SUFFIXES:
.SECONDARY:
%.c:;
Makefile:;
