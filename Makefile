BIN := softswitch

CC := gcc
LD := gcc
CFLAGS :=-g -O0
LDFLAGS :=

SRCS := softswitch.c \
	src/rts/xp.c \
	src/rts/xp_header.c

OBJS := $(patsubst %.c,%.o,$(SRCS))
DEPS := $(OBJS:.o=.d)

all: $(BIN)

clean:
	$(RM) $(OBJS) $(DEPS) $(BIN)

$(BIN): $(OBJS)
	$(LD) $(OBJS) $(LDFLAGS) -o $@

%.o: %.c
	$(CC) -c -MMD -MP $< -o $@ $(CFLAGS)

print-%: ; @echo $*=$($*)

-include $(DEPS)
