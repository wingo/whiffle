GUILE_MODULES = $(wildcard module/whiffle/*.scm)
GUILE_OBJS = $(GUILE_MODULES:%.scm=%.go)

TESTS = $(wildcard test/*.scm)
TEST_TARGETS = $(TESTS:%.scm=%.check)

GUILE = guile
GUILD = guild
GUILD_CFLAGS = -O2 -W2

all: $(GUILE_OBJS)

$(GUILE_OBJS): %.go: %.scm
	./pre-inst-env $(GUILD) compile $(GUILD_CFLAGS) -o $@ $<

clean:
	rm -f $(GUILE_OBJS)

check: $(TEST_TARGETS)

$(TEST_TARGETS): %.check: %.scm
	./pre-inst-env $(GUILE) $<

.PHONY: $(TEST_TARGETS) all check clean
