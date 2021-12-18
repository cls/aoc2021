CFLAGS   = -std=c99 -O3 -Wall -pedantic
CXXFLAGS = -std=c++11 -O3 -Wall -pedantic
LDFLAGS  = -s

BIN = bin/06_Lanternfish \
      bin/09_SmokeBasin \
      bin/11_DumboOctopus \
      bin/15_Chiton \
      bin/17_TrickShot

all: $(BIN)

clean:
	rm -f $(BIN)

bin:
	mkdir -p $@

bin/%: src/%.c | bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

bin/%: src/%.cpp | bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $<

.PHONY: all clean
