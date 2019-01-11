
CCFLAGS = -g -m32 -O2 -Wall -pedantic -std=c11

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif


all: repl.c 
	gcc $(CCFLAGS) repl.c ../build/linux-x86/library.a -o repl -I../include


clean:
	rm repl
