

CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11
CCFLAGS += -D_32_BIT_
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

all: $(EXECS)
	mv test_lisp_code.exe test_lisp_code
	mv test_lisp_code_mini_heap.exe test_lisp_code_mini_heap
	mv test_lisp_code_cps.exe test_lisp_code_cps
	mv test_lisp_code_cps_mini_heap.exe test_lisp_code_cps_mini_heap

%.exe: %.c
	$(CC) -I../include $(CCFLAGS) $< ../build/linux-x86/liblispbm.a -o $@ 



clean:
	rm *.exe
	rm test_lisp_code
	rm test_lisp_code_cps
	rm test_lisp_code_mini_heap
	rm test_lisp_code_cps_mini_heap

