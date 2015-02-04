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


.PHONY: all check clean


all: pc pi pb

%.c : %.c pc.h
	$(CC) $(SETTINGS) $(OPTFLAGS) $(CFLAGS) $< -o $@ $(LIBS)


clean:
	rm -f pc pi pb


check: all
	./pc pc.pl -o pc2.c
	cmp pc.c pc2.c
