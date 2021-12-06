CFLAGS = -O3 -std=c99 -Wall -pedantic
LDFLAGS = -s

BIN = bin/06_Lanternfish

all: $(BIN)

clean:
	rm -f $(BIN)

bin:
	mkdir -p $@

bin/%: src/%.c | bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

.PHONY: all clean
