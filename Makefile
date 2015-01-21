#### Makefile for pc


CC = gcc
CFLAGS = -std=gnu99 -I. -fno-strict-aliasing -fwrapv
OPTFLAGS = -O2 -fomit-frame-pointer
SETTINGS = -DTRAIL_STACK_SIZE=10000000 -DCHOICE_POINT_STACK_SIZE=100000000 \
	-DENVIRONMENT_STACK_SIZE=20000000 -DHEAP_SIZE=250000000
LIBS = -lm

ifeq ($(shell uname),Linux)
LIBS += -lrt
endif


.PHONY: all clean spotless


all: pc pi


pc: pc.c pc.h
	$(CC) $(SETTINGS) $(OPTFLAGS) $(CFLAGS) $< -o $@ $(LIBS)

pi: pi.c pc.h
	$(CC) $(SETTINGS) $(OPTFLAGS) $(CFLAGS) $< -o $@ $(LIBS)

clean:
	rm -f pc


spotless: clean
	rm pc.c pi.c
