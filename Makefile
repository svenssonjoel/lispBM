
ifndef PLATFORM
  BUILD_DIR = build/linux-x86
  CCFLAGS = -m32 -O2 -Wall -pedantic -std=c11
  #CCFLAGS += -DTINY_SYMTAB
endif

ifeq ($(PLATFORM),linux-x86-64)
  $(error Platform support not implemented)
  BUILD_DIR = build/linux-x86-64
  CCFLAGS = -O2 -DBUILD_X86_64
endif

ifeq ($(PLATFORM), zynq)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/zynq
  CCFLAGS = -mcpu=cortex-a9 -mfpu=vfpv3 -mfloat-abi=hard -O2 -Wall -pedantic
endif

ifeq ($(PLATFORM), stm32f4)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/stm32f4
  CCFLAGS = -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -O2 -Wall -pedantic -fmessage-length=0 -ffunction-sections -c -MMD -MP
  CCFLAGS += -DTINY_SYMTAB
endif

ifeq ($(PLATFORM), nrf52840_pca10056)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/nrf52840_pca10056
  CCFLAGS =  -DNRF52840_XXAA -D_FORTIFY_SOURCE=2  -nostdinc -g -Wall -Wformat -Wformat-security -Wno-format-zero-length  -ffreestanding -Wno-main -fno-common -mthumb -mcpu=cortex-m4 -fno-asynchronous-unwind-tables -fno-pie -fno-pic -fno-strict-overflow -Wno-pointer-sign -Wno-unused-but-set-variable -fno-reorder-functions -fno-defer-pop -Werror=implicit-int -Wpointer-arith -ffunction-sections -fdata-sections -mabi=aapcs -march=armv7e-m -std=c99
  CCFLAGS += -DTINY_SYMTAB
endif

ifeq ($(PLATFORM), pi)
  $(error Platform support not implemented)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = /build/pi
  CCFLAGS = 
endif

SOURCE_DIR = src
INCLUDE_DIR = include

$(shell mkdir -p ${BUILD_DIR})

CC=${CROSS_COMPILE}gcc
AR=${CROSS_COMPILE}ar

SRC = src
OBJ = obj

SOURCES = $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS = $(patsubst $(SOURCE_DIR)/%.c, $(BUILD_DIR)/%.o, $(SOURCES))

ifdef HEAP_VIS
	OBJECTS += $(BUILD_DIR)/heap_vis.o
	CCFLAGS += -DVISUALIZE_HEAP
endif


LIB = $(BUILD_DIR)/library.a

all: $(OBJECTS) $(LIB)

$(LIB): $(OBJECTS)
	$(AR) -rcs $@ $(OBJECTS)

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.c
	$(CC) -I$(INCLUDE_DIR) $(CCFLAGS) -c $< -o $@


$(BUILD_DIR)/heap_vis.o: $(SOURCE_DIR)/visual/heap_vis.c
	$(CC) -I$(INCLUDE_DIR) $(CCFLAGS) -c $< -o $@


clean:
	rm -f ${BUILD_DIR}/*.o
	rm -f ${BUILD_DIR}/*.a

