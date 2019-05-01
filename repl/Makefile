

all: repl.c 
	gcc -m32 -O2 -Wall -pedantic -std=c11 -D_32_BIT_ repl.c ../build/linux-x86/liblispbm.a -o repl -I../include


clean:
	rm repl
