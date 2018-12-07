
CCFLAGS = -m32 -O2

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif


all: repl.c 
	gcc $(CCFLAGS) repl.c ../build/linux-x86/library.a -o repl -I../include
