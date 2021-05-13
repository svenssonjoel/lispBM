
LISPBM := ../

LISPBM_USE_COMPRESSION = true
LISPBM_USE_EVAL_CPS = true
LISPBM_USE_PRELUDE = true
LISPBM_USE_EXTENSIONS = true

include $(LISPBM)/lispbm.mk

$(warning $(LISPBM_SRC))

CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

#LIB = ../build/linux-x86/liblispbm.a -lpthread

all: repl

debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LISPBM_SRC)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(LISPBM_FLAGS) repl.c -o repl -I../include -lpthread

$(LIB):
	@make -C ..


clean:
	rm repl
