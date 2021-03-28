# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.13.1)
include($ENV{ZEPHYR_BASE}/cmake/app/boilerplate.cmake NO_POLICY_SCOPE)
project(repl)

add_definitions(-D_32_BIT_ -D_PRELUDE)

add_custom_command(OUTPUT ../src/prelude.xxd 
                   COMMAND xxd
		   ARGS -i < ../src/prelude.lisp > ../src/prelude.xxd
		   DEPENDS ../src/prelude.lisp
                  )
		
FILE(GLOB app_sources src/*.c)
FILE(GLOB lisp_sources ../src/*.c)

FILE(GLOB llbridge_zephyr_sources ../../LLBridge/common/src/abstraction/zephyr/*.c)
FILE(GLOB llbridge_sources ../../LLBridge/common/src/*.c)

target_sources(app PRIVATE ${app_sources}
		   PRIVATE ${lisp_sources}
		   PRIVATE ../src/prelude.xxd
		   PRIVATE ${llbridge_sources}
		   PRIVATE ${llbridge_zephyr_sources})
		 
target_include_directories(app PRIVATE ../include
                               PRIVATE ../src
                               PRIVATE ../../LLBridge/common/src/abstraction/zephyr
                               PRIVATE ../../LLBridge/common/src
			       ${ZEPHYR_BASE}/kernel/include
			       ${ZEPHYR_BASE}/arch/${ARCH}/include)


