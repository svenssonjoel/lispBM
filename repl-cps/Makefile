
LISPBM := ../

LISPBM_USE_COMPRESSION = true
LISPBM_USE_EVAL_CPS = true
LISPBM_USE_PRELUDE = true
LISPBM_USE_EXTENSIONS = true

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11

PICCFLAGS = -O2 -Wall -Wconversion -pedantic -std=c11

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

all: repl $(LISPBM_DEPS)

debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LISPBM_SRC)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl -I../include $(PLATFORM_INCLUDE) -lpthread

pirepl: repl.c $(LISPBM_SRC)
	gcc $(PICCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl -I../include $(PLATFORM_INCLUDE) -lpthread

clean:
	rm repl
