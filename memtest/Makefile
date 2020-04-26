
CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11 

LIB = ../build/linux-x86/liblispbm.a -lpthread

all: mem

debug: CCFLAGS += -g
debug: mem

mem: mem.c $(LIB)
	gcc $(CCFLAGS) mem.c $(LIB) -o mem -I../include

$(LIB):
	@make -C ..


clean:
	rm mem
