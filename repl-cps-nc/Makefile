
CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

LIB = ../build/linux-x86/liblispbm.a

all: repl

debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LIB)
	gcc $(CCFLAGS) repl.c $(LIB) -o repl -I../include

$(LIB):
	@make -C ..


clean:
	rm repl
