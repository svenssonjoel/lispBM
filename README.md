# lispBM

A lisp-like language (work in progress) implemented in C using the MPC (https://github.com/orangeduck/mpc) library for the parsing task.

## Purpose
1. Have fun.
2. Learn about lisp.
3. An interactive REPL for Zynq devboard.
4. ...

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. A few built-in functions: cons, car, cdr, eval, list, gensym, +, -, >, < and =
3. Some special forms: Lambdas, closures, lets (letrecs), define and quote.
4. Compiles for, and runs on linux-x86 (builds 32bit library, runs on 32/64 bit).
5. Compiles for, and runs on Zynq 7000.  

## In progress
1. a new evaluator that allows tail-recursive programs to execute in constant space. 

## Short term TODOs
1. Clean up a bit. 
2. Write more tests.
3. Test the Zynq build more. 
4. Document code.
5. Implement the boxed types (float, int32, uint32, Vector, String etc).
6. Implement some looping structure (since recursion is very limited).
   Maybe not so limited anymore. Built in loops could be added for speed though.. 
7. Be much more stringent on checking of error conditions etc. 

## Compile for linux
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

6. Go to menu: Xilinx > Generate linker script
   and set up for enough of heap and stack. 128MB heap and 16MB stack is an ok starting point.

7. Remove the helloworld.c file from the project and replace with repl-zynq/repl.c

8. build and run. 
