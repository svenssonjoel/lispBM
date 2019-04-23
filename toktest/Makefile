

all: toktest.c 
	gcc -m32 -O2 -Wall -pedantic -std=c11 -D_32_BIT_ toktest.c ../build/linux-x86/library.a -o toktest -I../include


clean:
	rm toktest
