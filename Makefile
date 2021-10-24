
ifndef PLATFORM
  BUILD_DIR = build/linux-x86
  CCFLAGS = -g -O2 -m32 -Wall -Wextra -pedantic -std=c11
  CCFLAGS += -D_PRELUDE 
  CC=gcc
  AR=ar
else
  CC=${CROSS_COMPILE}gcc
  AR=${CROSS_COMPILE}ar
endif

ifeq ($(PLATFORM),linux-x86-64)
  $(error WILL NOT SUPPORT 64bit platforms)
endif

ifeq ($(PLATFORM), zynq)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/zynq
  CCFLAGS = -mcpu=cortex-a9 -mfpu=vfpv3 -mfloat-abi=hard -O2 -Wall -Wextra -pedantic
  CCFLAGS += -D_PRELUDE
endif

ifeq ($(PLATFORM), stm32f4)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/stm32f4
  CCFLAGS = -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -O2 -Wall -Wextra -pedantic
#-fmessage-length=0 -ffunction-sections -c -MMD -MP
  CCFLAGS += -D_PRELUDE
endif

ifeq ($(PLATFORM), nrf52840)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/nrf52840
  CCFLAGS =  -mcpu=cortex-m4  -mthumb -ffunction-sections -fdata-sections -mabi=aapcs -march=armv7e-m -O2 -Wall -Wextra -pedantic
  CCFLAGS += -D_PRELUDE
endif

ifeq ($(PLATFORM), pi)
  $(error Platform support not implemented)
  CROSS_COMPILE = aarch32
  BUILD_DIR = /build/pi
  CCFLAGS += -D_PRELUDE
endif

SOURCE_DIR = src
INCLUDE_DIR = include

$(shell mkdir -p ${BUILD_DIR})

SRC = src
OBJ = obj

SOURCES = $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS = $(patsubst $(SOURCE_DIR)/%.c, $(BUILD_DIR)/%.o, $(SOURCES))

ifdef HEAP_VIS
	OBJECTS += $(BUILD_DIR)/heap_vis.o
	CCFLAGS += -DVISUALIZE_HEAP
endif


LIB = $(BUILD_DIR)/liblispbm.a

all: $(OBJECTS) $(LIB)

debug: CCFLAGS += -g 
debug: $(OBJECTS) $(LIB)

$(LIB): $(OBJECTS) 
	$(AR) -rcs $@ $(OBJECTS)

src/prelude.xxd: src/prelude.lisp
	xxd -i < src/prelude.lisp > src/prelude.xxd 

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.c src/prelude.xxd
	$(CC) -I$(INCLUDE_DIR) $(CCFLAGS) -c $< -o $@


$(BUILD_DIR)/heap_vis.o: $(SOURCE_DIR)/visual/heap_vis.c
	$(CC) -I$(INCLUDE_DIR) $(CCFLAGS) -c $< -o $@


clean:
	rm src/prelude.xxd
	rm -f ${BUILD_DIR}/*.o
	rm -f ${BUILD_DIR}/*.a

