

CCFLAGS = -m32 -O2 
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

all: $(EXECS)
	mv test_lisp_code.exe test_lisp_code
	mv test_lisp_code_cps.exe test_lisp_code_cps
	mv test_lisp_code_cps_mini_heap.exe test_lisp_code_cps_mini_heap

%.exe: %.c
	$(CC) -I../include $(CCFLAGS) $< ../build/linux-x86/library.a -o $@ 



clean:
	rm *.exe
	rm test_lisp_code 
