

CCFLAGS = -m32 -O2 
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

all: $(EXECS)

%.exe: %.c
	$(CC) -I../src $(CCFLAGS) $< ../build/linux-x86/library.a -o $@ 
