
CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11 -D_32_BIT_

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif


all: repl.c 
	gcc $(CCFLAGS) repl.c ../build/linux-x86/liblispbm.a -o repl -I../include


clean:
	rm repl
