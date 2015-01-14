#### Makefile for pc


CC = gcc
CFLAGS = -std=gnu99 -I. -fno-strict-aliasing -fwrapv
OPTFLAGS = -O2 -fomit-frame-pointer
LIBS = -lm -lrt


.PHONY: all clean spotless


all: pc


pc: pc.c pc.h
	$(CC) $(OPTFLAGS) $(CFLAGS) $< -o $@


clean:
	rm -f pc


spotless: clean
	rm pc.c
