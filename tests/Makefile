

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

#CCFLAGS = -g -O2 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99

# -DLBM_ALWAYS_GC

LBMFLAGS = -DFULL_RTS_LIB -DLBM_USE_DYN_FUNS -DLBM_USE_DYN_MACROS -DLBM_USE_DYN_LOOPS -DLBM_USE_DYN_ARRAYS
LBM_SIZE = -DLBM_OPT_FUNDAMENTALS_SIZE -DLBM_OPT_ARRAY_EXTENSIONS_SIZE -DLBM_OPT_DISPLAY_EXTENSIONS_SIZE -DLBM_OPT_MATH_EXTENSIONS_SIZE -DLBM_OPT_MUTEX_EXTENSIONS_SIZE -DLBM_OPT_RANDOM_EXTENSIONS_SIZE -DLBM_OPT_RUNTIME_EXTENSIONS_SIZE -DLBM_OPT_SET_EXTENSIONS_SIZE -DLBM_OPT_STRING_EXTENSIONS_SIZE -DLBM_OPT_TTF_EXTENSIONS_SIZE
LBM_SIZE_AGGRESSIVE = -DLBM_OPT_FUNDAMENTALS_SIZE_AGGRESSIVE -DLBM_OPT_ARRAY_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_DISPLAY_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_MATH_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_MUTEX_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_RANDOM_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_RUNTIME_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_SET_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_STRING_EXTENSIONS_SIZE_AGGRESSIVE -DLBM_OPT_TTF_EXTENSIONS_SIZE_AGGRESSIVEa

CCFLAGS = -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99 $(LBMFLAGS)

CCFLAGS_32 = $(CCFLAGS) -m32 -g -O2
CCFLAGS_GC = $(CCFLAGS) -m32 -DLBM_ALWAYS_GC -g -O2
CCFLAGS_REVGC = $(CCFLAGS) -DLBM_USE_GC_PTR_REV -m32
CCFLAGS_64 = $(CCFLAGS) -DLBM64 -g -O2
CCFLAGS_COV_32 = $(CCFLAGS) -m32 --coverage -g -O0 -DLONGER_DELAY
CCFLAGS_COV_64 = $(CCFLAGS) -DLBM64 --coverage -g -O0 -DLONGER_DELAY
CCFLAGS_TIME_32 = $(CCFLAGS) -m32 -g -O2 -DLBM_USE_TIME_QUOTA
CCFLAGS_TIME_64 = $(CCFLAGS) -DLBM64 -g -O2 -DLBM_USE_TIME_QUOTA

CC=gcc

SRC = src

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

test_lisp_code_cps_gc: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_GC) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_gc -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_cov: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_COV_32) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_cov -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_COV_32) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_size: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_32) $(LBM_SIZE) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_size -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_size_aggressive: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_32) $(LBM_SIZE_AGGRESSIVE) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_size_aggressive -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_time: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_TIME_32) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_time -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_64: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_COV_64) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_64 -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_64_time: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_TIME_64) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_64_time -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_revgc: $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_H) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_REVGC) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_revgc -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

all: test_lisp_code_cps_cov test_lisp_code_cps test_lisp_code_cps_64 test_lisp_code_cps_revgc test_lisp_code_cps_gc

clean:
	rm -f *.exe
	rm -f test_lisp_code_cps
	rm -f test_lisp_code_cps_64
	rm -f test_lisp_code_cps_gc
	rm -f test_lisp_code_cps_revgc
	rm -f test_lisp_code_cps_cov
	rm -f test_heap_alloc
	rm -f *.gcda
	rm -f *.gcno

