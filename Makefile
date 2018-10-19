
BUILD_DIR := build/linux-x86
SOURCE_DIR := src

ifeq ($(PLATFORM), ZYNQ)
  CROSS_COMPILE=arm-none-eabi-
  BUILD_DIR := build/zynq
endif

ifeq ($(PLATFORM), PI)
  CROSS_COMPILE=arm-none-eabi-
  BUILD_DIR := /build/pi
endif

$(shell mkdir -p ${BUILD_DIR})

CC=${CROSS_COMPILE}gcc
AR=${CROSS_COMPILE}ar

SRC := src
OBJ := obj

SOURCES := $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS := $(patsubst $(SOURCE_DIR)/%.c, $(BUILD_DIR)/%.o, $(SOURCES))

all: $(OBJECTS)

## Build all o's into an executable. 
# $(CC) $^ -o $@

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.c
	$(CC) -I$(SOURCE_DIR) -c $< -o $@


clean:
	rm ${BUILD_DIR}/*
