first_rule: all

include $(LISPBM)/src/lispbm.mk

LISPBM_SRC = $(LISPBM_BASE_SRC)
LISPBM_H   = $(LISPBM)/include
LISPBM_FLAGS =
LISPBM_DEPS  = 

ifeq ($(LISPBM_USE_COMPRESSION), true)
LISPBM_SRC += $(LISPBM_COMPRESSION_SRC)
endif

ifeq ($(LISPBM_USE_EC_EVAL), true)
LISPBM_SRC += $(LISPBM_EC_EVAL_SRC)
endif

ifeq ($(LISPBM_USE_EVAL_CPS), true)
LISPBM_SRC += $(LISPBM_EVAL_CPS_SRC)
endif

ifeq ($(LISPBM_USE_PRELUDE), true)
LISPBM_SRC += $(LISPBM_PRELUDE_SRC)
LISPBM_FLAGS += -D_PRELUDE

LISPBM_DEPS += $(LISPBM)/src/prelude.xxd

$(LISPBM)/src/prelude.xxd: $(LISPBM)/src/prelude.lisp
	xxd -i < $(LISPBM)/src/prelude.lisp > $(LISPBM)/src/prelude.xxd 

endif

ifeq ($(LISPBM_USE_EXTENSIONS), true)
LISPBM_SRC += $(LISPBM_EXTENSIONS_SRC)
endif 
