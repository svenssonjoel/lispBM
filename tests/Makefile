

CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11 
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

all: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps
	mv test_lisp_code_cps_nc.exe test_lisp_code_cps_nc

%.exe: %.c
	$(CC) -I../include $(CCFLAGS) $< ../build/linux-x86/liblispbm.a -o $@  -lpthread


clean:
	rm *.exe
	rm test_lisp_code_cps

