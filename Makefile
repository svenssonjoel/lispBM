
BUILD_DIR=./build/linux-x86


ifeq ($(PLATFORM), ZYNQ)
  CROSS_COMPILE=arm-none-eabi-
  BUILD_DIR=./build/zynq
endif

ifeq ($(PLATFORM), PI)
  CROSS_COMPILE=arm-none-eabi-
  BUILD_DIR=./build/pi
endif

$(shell mkdir -p ${BUILD_DIR})

CC=${CROSS_COMPILE}gcc
AR=${CROSS_COMPILE}ar

INCLUDE = ./Frontend

OBJS = ${BUILD_DIR}/mpc.o

all: ${BUILD_DIR}/parse1
	@echo "Using gcc from: " $(CROSS_COMPILE)
	@echo "target files in: " $(BUILD_DIR)


${BUILD_DIR}/mpc.o:
	${CC} -c ./Frontend/mpc.c -o ${BUILD_DIR}/mpc.o -I$(INCLUDE)


${BUILD_DIR}/parse1: ${OBJS}
	${CC} ./Test/parse1.c -o ${BUILD_DIR}/parse1 ${BUILD_DIR}/*.o -I$(INCLUDE)


clean:
	rm ${BUILD_DIR}/*
