
ifndef PLATFORM
  BUILD_DIR = build/linux-x86
  SOURCE_DIR = src
  CCFLAGS = 
endif

ifeq ($(PLATFORM), zynq)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = build/zynq
  CCFLAGS = -mcpu=cortex-a9 -mfpu=vfpv3 -mfloat-abi=hard
endif

ifeq ($(PLATFORM), pi)
  CROSS_COMPILE = arm-none-eabi-
  BUILD_DIR = /build/pi
  CCFLAGS = 
endif

$(shell mkdir -p ${BUILD_DIR})

CC=${CROSS_COMPILE}gcc
AR=${CROSS_COMPILE}ar

SRC = src
OBJ = obj

SOURCES = $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS = $(patsubst $(SOURCE_DIR)/%.c, $(BUILD_DIR)/%.o, $(SOURCES))

LIB = $(BUILD_DIR)/library.a

#all: $(OBJECTS) $(LIB)

lib: $(LIB)
## Build all o's into an executable. 
# $(CC) $^ -o $@

test: $(SOURCE_DIR)/mpc.c $(SOURCE_DIR)/parse.c $(SOURCE_DIR)/heap0.c $(SOURCE_DIR)/symtab.c
	gcc -m32 -c $(SOURCE_DIR)/mpc.c $(SOURCE_DIR)/parse.c $(SOURCE_DIR)/heap0.c $(SOURCE_DIR)/symtab.c 

test-lib: mpc.o parse.o heap0.o symtab.o
	ar -rcs library.a mpc.o parse.o heap0.o symtab.o

$(LIB): $(OBJECTS)
	$(AR) -rcs $@ $(OBJECTS)

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.c
	$(CC) -I$(SOURCE_DIR) $(CCFLAGS) -c $< -o $@


clean:
	rm -f ${BUILD_DIR}/*.o
	rm -f ${BUILD_DIR}/*.a

