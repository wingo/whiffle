GUILE_MODULES = $(wildcard module/whiffle/*.scm)
GUILE_MODULE_OBJS = $(GUILE_MODULES:%.scm=%.go)

GUILD = guild
GUILD_CFLAGS = -O2 -W2

all: $(GUILE_MODULE_OBJS)

$(GUILE_MODULE_OBJS): %.go: %.scm
	./pre-inst-env $(GUILD) compile $(GUILD_CFLAGS) -o $@ $<
