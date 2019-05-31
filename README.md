# lispBM [![Build Status](https://travis-ci.org/svenssonjoel/lispBM.svg?branch=master)](https://travis-ci.org/svenssonjoel/lispBM)

A lisp-like language (work in progress) implemented in C for 32-bit platforms.

## Purpose
1. Have fun.
2. Learn about lisp.
3. An interactive REPL for devboards.
4. ...

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. Built-in functions: cons, car, cdr, eval, list, gensym, +, -, >, <, = and more.
3. Some special forms: Lambdas, closures, lets (letrecs), define and quote.
4. 28-Bit signed/unsigned integers and boxed 32-Bit Float, 32-Bit signed/unsigned values.
5. Arrays (in progress), string is an array. 
6. Compiles for, and runs on linux-x86 (builds 32bit library, runs on 32/64 bit).
7. Compiles for, and runs on Zynq 7000.
8. Compiles for, and runs on STM32f4. 

## TODOs
1. (DONE) Write some tests that stresses the Garbage collector.
2. (DONE) Implement some "reference to X type", for uint32, int32. 
3. (DONE) Write a small library of useful hofs. 
4. Implement some looping structure for speed or just ease of use. 
5. Be much more stringent on checking of error conditions etc.
6. Improve handling of arguments in eval-cps. 
7. Code improvements with simplicity, clarity  and readability in mind.
8. (DONE) Implement a small dedicated lisp reader/parser to replace MPC. MPC eats way to much memory for small platforms.
9. (DONE) Port to STM32f4 - 128K ram platform (will need big changes). (surely there will be some more bugs)
10. (DONE) Add STM32f4 example code (repl implementation)
11. Port to nrf52840_pca10056 - 256k ram platform (same changes as above).
12. Port to Raspberry pi 32Bit Bare-metal.
13. Reduce size of builtins.c and put platform specific built in functions elsewhere.
14. (DONE, for eval_cps) Implement 'progn' facility.
15. Add facilities for "pre-parsing" and for uploading "heap-images" to a running system.
16. See if it is possible to explicitly free heap values that are used as temporaries when evaluating. 
17. (DONE) Remove the "gensym" functionality havent found a use for it so far and it only complicates things.
18. Test on all platforms after big changes to eval_cps.c.
19. Bytecode compilation of functions (with restrictions).

## Compile for linux (Requires 32bit libraries. May need something like "multilib" on a 64bit linux)
1. Build the library: `make`

2. Build the repl: `cd repl` and then `make`

3. Run the repl: `./repl`

## Compile for Zynq devboard (bare-metal)
1. Source your vivado settings: `source <PATH_TO>/settings.sh`

2. Build library for ARM A9: `PLATFORM=zynq make`

3. Create a standalone app in Vivado SDK (helloworld template is a good starting point) 

4. Go to menu: Project > Properties > Tool settings > ARM v7 gcc linker > Miscellaneous
   and add in OTHER OBJECTS the path to liblispbm.a (build/zynq/liblispbm.a)

5. Go to menu: Project > Properties > Tool settings > ARM v7 gcc compiler > Directories
   and add in INCLUDE DIRECTORIES the path to the include directory.

6. Go to menu: Project > Properties > Tool settings > ARM v7 gcc compiler > Miscellaneous
    and add `-D_32_BIT_` to "Other flags"

7. Go to menu: Xilinx > Generate linker script
   and set up for enough of heap and stack. 128MB heap and 16MB stack is an ok starting point.

8. Remove the helloworld.c file from the project and replace with repl-zynq/repl.c

9. build and run. 
