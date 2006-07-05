CC = gcc
CFLAGS = -DNDEBUG -O2 `xml2-config --cflags`
LIBS = `xml2-config --libs`

all: genSplit

genSplit: genSplit.o
	$(CC) -o $@ $^ $(LIBS)

genSplit.o: genSplit.c
	$(CC) -c $(CFLAGS) -o $@ $< 

.PHONY: clean
clean:
	-rm -f *.o *~
