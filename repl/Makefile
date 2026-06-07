
PLATFORM ?=
FEATURES ?=

LISPBM := ../
FEATURE_FILE = .build_features
.PHONY: FORCE all help install clean clean_coverage

include $(LISPBM)/lispbm.mk

CCFLAGS =

LBMFLAGS = -DFULL_RTS_LIB \
           -DLBM_USE_DYN_MACROS \
           -DLBM_USE_DYN_LOOPS \
           -DLBM_USE_DYN_FUNS \
           -DLBM_USE_DYN_ARRAYS \
           -DLBM_USE_DYN_DEFSTRUCT \
           -DLBM_USE_TIME_QUOTA \
           -DLBM_USE_ERROR_LINENO \
           -DLBM_USE_MACRO_REST_ARGS \
           -DLBM_USE_SHEBANG_COMMENTS

LDFLAGS = -lpng

# Detect OS and Architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Platform-specific paths
ifeq ($(UNAME_S),Darwin)
	HOMEBREW      = /opt/homebrew
	SDL_PATH      = $(HOMEBREW)/include/SDL2
	FREETYPE_PATH = $(HOMEBREW)/include/freetype2
	RTLSDR_PATH   = $(HOMEBREW)/include
	FTDI_PATH     = $(HOMEBREW)/include/libftdi1
	LIB_PATH      = $(HOMEBREW)/lib
	PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include \
	                   -I$(HOMEBREW)/include \
	                   -I$(HOMEBREW)/opt/readline/include
	LDFLAGS += -L$(HOMEBREW)/opt/readline/lib -lreadline -L$(LIB_PATH)
	LBMFLAGS += -D_DARWIN_C_SOURCE
	FEATURES += 64
else
	SDL_PATH      = /usr/include/SDL2
	FREETYPE_PATH = /usr/include/freetype2
	RTLSDR_PATH   = /usr/include
	FTDI_PATH     = /usr/include/libftdi1
	PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
	LDFLAGS += -lpthread -lreadline -lhistory
	CCFLAGS += -fno-pie -no-pie
endif

PLATFORM_SRC = $(LISPBM)/platform/linux/src/platform_mutex.c \
               $(LISPBM)/platform/linux/src/platform_timestamp.c \
               $(LISPBM)/platform/linux/src/platform_thread.c

## If using any of the utilities.
LISPBM_INC += -I../utils

REPL_SRC = $(LISPBM_SRC)\
           repl.c \
           repl_exts.c \
           ../utils/crc.c \
           ../utils/packet.c \
           vesc_express_extension_stubs.c \
           lbm_gnuplot.c \
           lbm_octave.c \
           bldc_extension_stubs.c

## Dropped usage of pkg-config as it felt like more trouble than benefit.
## pkg-config searches for .pc files unrelated to compile flags such as -m32.
## Using just -lasound (for example) means that the linker will look for
## the alsa libraries while taking flags such as -m32 into consideration.

# Coverage build or regular
ifneq (,$(filter coverage,$(FEATURES)))
	CCFLAGS += -g -coverage -O0 -Wall -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS)
else
	CCFLAGS += -O2 -Wall -Wextra -Wshadow  -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS) -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast
endif

# Use 64bit
ifneq (,$(filter 64, $(FEATURES)))
	CCFLAGS += -DLBM64
else
	CCFLAGS += -m32
endif

# Use SDL
ifneq (,$(filter sdl,$(FEATURES)))
	CCFLAGS += -DWITH_SDL -I$(SDL_PATH)
	LDFLAGS += -lSDL2 -lSDL2_image
	REPL_SRC += lbm_sdl.c
endif

# Use ALSA
ifneq (,$(filter alsa, $(FEATURES)))
	CCFLAGS += -DWITH_ALSA
	LDFLAGS += -lasound
	REPL_SRC += lbm_sound.c lbm_midi.c
endif

# Use freetype
ifneq (,$(filter freetype, $(FEATURES)))
	CCFLAGS += -DLBM_TTF_USE_FREETYPE -I$(FREETYPE_PATH)
	LDFLAGS += -lfreetype
	REPL_SRC := $(filter-out %/schrift.c,$(REPL_SRC))
	REPL_SRC += $(LISPBM)/src/extensions/ttf_backend_freetype.c
endif

# USE Debug info
ifneq (,$(filter debug, $(FEATURES)))
	CCFLAGS += -g
endif

# USE rtlsdr extensions
ifneq (,$(filter rtlsdr, $(FEATURES)))
	CCFLAGS += -DWITH_RTLSDR -I$(RTLSDR_PATH)
	LDFLAGS += -lrtlsdr
	REPL_SRC += lbm_rtlsdr.c
endif

ifneq (,$(filter ft4232h_nand, $(FEATURES)))
	CCFLAGS  += -DTEST_FT4232H_NAND_DRIVER -I$(FTDI_PATH)
	LDFLAGS  += -lftdi1
	REPL_SRC += ft4232h_w25n01.c
endif

ifneq (,$(filter ft232h_nand, $(FEATURES)))
	CCFLAGS  += -DTEST_FT232H_NAND_DRIVER -I$(FTDI_PATH)
	LDFLAGS  += -lftdi1
	REPL_SRC += ft232h_w25n01.c
endif

# MCP (Model Context Protocol) stdio server
ifneq (,$(filter mcp, $(FEATURES)))
	CCFLAGS  += -DWITH_MCP -I../utils/mcp
	REPL_SRC += ../utils/mcp/lbm_mcp.c ../utils/mcp/cJSON.c
endif

# -Wjump-misses-init
# -fsanitize=address

#improved_closures:
#improved_closures: repl clean_cl.h
#clean_cl.h: ./scripts/clean.lisp
#	./repl --store_env="clean_cl.env" --src=./scripts/clean.lisp --terminate
#	xxd -i clean_cl.env clean_cl.h

all: repl

help:
	@echo "Run 'make' to build the REPL"
	@echo ""
	@echo "Add a 'FEATURES' list to select REPL features:"
	@echo "64       - Build 64Bit LispBM REPL"
	@echo "alsa     - Sound and midi, builds the LispBM soft synth."
	@echo "sdl      - Adds SDL support for graphics."
	@echo "freetype - Use libfreetype for font preprocessing."
	@echo "coverage  - Build with coverage information collection."
	@echo "mcp       - Build with MCP (Model Context Protocol) server support. Adds --mcp flag."
	@echo "debug     - Build with debug symbols."
	@echo "test_nand - Build FT4232H + W25N01GVZEIG NAND driver test (requires libftdi1-dev)."
	@echo ""
	@echo "Example: 'make FEATURES=\"alsa sdl\"' to build with sound and graphics support."
	@echo "Example: 'make FEATURES=\"sdl freetype\"' to build with graphics and freetype support."

install:
	@if [ ! -f repl ]; then echo "Error: repl not built. Run 'make help' for information." ; exit 1; fi
	mkdir -p ~/.local/bin
	@if echo "$(FEATURES)" | grep -qw "64"; then \
		cp repl ~/.local/bin/lbm64; \
		echo "Installed as lbm64"; \
	else \
		cp repl ~/.local/bin/lbm; \
		echo "Installed as lbm"; \
	fi
	cp shlbm ~/.local/bin/shlbm
	chmod a+x ~/.local/bin/shlbm

repl: $(FEATURE_FILE) $(REPL_SRC) $(PLATFORM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(CCFLAGS) $(PLATFORM_SRC) $(REPL_SRC) -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) $(LDFLAGS) $(LISPBM_FLAGS)

clean:
	rm -f repl
	rm -f .build_features
	rm -f *.gcda
	rm -f *.gcno

clean_coverage:
	rm -f coverage/*

$(FEATURE_FILE): FORCE
	@echo "$(FEATURES)" | cmp -s - $@ || echo "$(FEATURES)" > $@
