# lispBM

A lisp-like language (work in progress) implemented in C using the MPC (https://github.com/orangeduck/mpc) library for the parsing task.

## Purpose
1. Have fun.
2. Learn about lisp.
3. An interactive REPL for Zynq devboard.
4. ...

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. Built-in functions: cons, car, cdr, eval, list, gensym, +, -, >, <, = and more.
3. Some special forms: Lambdas, closures, lets (letrecs), define and quote.
4. 28-Bit signed/unsigned integers and (boxed) 32-Bit Float, Signed and Unsigned values.
5. Arrays (in progress), string is an array. 
6. Compiles for, and runs on linux-x86 (builds 32bit library, runs on 32/64 bit).
7. Compiles for, and runs on Zynq 7000.  

## Short term TODOs
0. Explicit stack in eval.c to help GC do its job. 
1. Write some tests that stresses the Garbage collector.
2. Implement some "reference to X type", for uint32, int32, float perhaps...  
3. Write a small library of useful hofs. 
4. Document code.
5. Implement some looping structure for speed or just ease of use. 
6. Be much more stringent on checking of error conditions etc.
7. Improve handling of arguments in eval-cps. 
8. Code improvements with simplicity, clarity  and readability in mind.

## Medium term TODOs
0. Implement a small dedicated lisp reader/parser to replace MPC. MPC eats way to much memory for small platforms.
1. Port to STM32f4 - 128K ram platform (will need big changes).
2. Port to nrf52840_pca10056 - 256k ram platform (same changes as above).

## Longer term TODOs
0. Port to X86_64 linux. (going 64bit involves some big changes) 
1. Port to Raspberry pi 64Bit Bare-metal.

## Compile for linux (Requires 32bit libraries. May need something like "multilib" on a 64bit linux)
1. Build the library: `make`

2. Build the repl: `cd repl` and then `make`

3. Run the repl: `./repl`

## Compile for Zynq devboard (bare-metal)
1. Source your vivado settings: `source <PATH_TO>/settings.sh`

2. Build library for ARM A9: `PLATFORM=zynq make`

3. Create a standalone app in Vivado SDK (helloworld template is a good starting point) 

4. Go to menu: Project > Properties > Tool settings > ARM v7 gcc linker > Miscellaneous
   and add in OTHER OBJECTS the path to library.a (build/zynq/library.a)

5. Go to menu: Project > Properties > Tool settings > ARM v7 gcc compiler > Directories
   and add in INCLUDE DIRECTORIES the path to the include directory.

6. Go to menu: Project > Properties > Tool settings > ARM v7 gcc compiler > Miscellaneous
    and add `-D_32_BIT_` and `-std=c11` to "Other flags"

7. Go to menu: Xilinx > Generate linker script
   and set up for enough of heap and stack. 128MB heap and 16MB stack is an ok starting point.

8. Remove the helloworld.c file from the project and replace with repl-zynq/repl.c

9. build and run. 
