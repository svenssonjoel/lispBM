

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

#CCFLAGS = -g -O2 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99
CCFLAGS = -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99 -DFULL_RTS_LIB
# -DLBM_ALWAYS_GC
CC=gcc

SRC = src

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

gc: CCFLAGS += -m32 -DLBM_ALWAYS_GC -g -O2
gc: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps_gc


test_lisp_code_cps_cov: CCFLAGS += -m32 --coverage -g -O0 -DLONGER_DELAY
test_lisp_code_cps_cov: 
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_cov -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps: CCFLAGS += -m32 -g -O2
test_lisp_code_cps: 
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_64: CCFLAGS += -DLBM64 -g -O2
test_lisp_code_cps_64: 
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_64 -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_revgc: CCFLAGS += -DUSE_GC_PTR_REV -m32
test_lisp_code_cps_revgc: 
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_revgc -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

all: test_lisp_code_cps_cov test_lisp_code_cps test_lisp_code_cps_64 test_lisp_code_cps_revgc

clean:
	rm -f *.exe
	rm -f test_lisp_code_cps
	rm -f test_lisp_code_cps_64
	rm -f test_lisp_code_cps_gc
	rm -f test_lisp_code_cps_revgc
	rm -f test_heap_alloc
	rm -f *.gcda
	rm -f *.gcno

