
LISPBM_BASE_SRC := $(LISPBM)/src/env.c \
                   $(LISPBM)/src/fundamental.c \
                   $(LISPBM)/src/heap.c \
                   $(LISPBM)/src/memory.c \
                   $(LISPBM)/src/print.c \
                   $(LISPBM)/src/qq_expand.c \
                   $(LISPBM)/src/stack.c \
                   $(LISPBM)/src/symrepr.c \
                   $(LISPBM)/src/tokpar.c

LISPBM_COMPRESSION_SRC := $(LISPBM)/src/compression.c

LISPBM_EC_EVAL_SRC := $(LISPBM)/src/ec_eval.c

LISPBM_EVAL_CPS_SRC := $(LISPBM)/src/eval_cps.c

LISPBM_PRELUDE_SRC := $(LISPBM)/src/prelude.c 

LISPBM_EXTENSIONS_SRC := $(LISPBM)/src/extensions.c
