# lispBM

A lisp-like language (work in progress) implemented in C using the MPC (https://github.com/orangeduck/mpc) library for the parsing task.

## Purpose
1. Have fun.
2. Learn about lisp.
3. ...

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. A few built-in functions: define, cons, car, cdr, eval, list, gensym, +, -, >, < and =
3. Some special forms: Lambdas, closures, lets (letrecs) and quote.
4. Compiles for, and runs on linux-x86 (builds 32bit library, runs on 32/64 bit).
5. Compiles for, and runs on Zynq 7000.  

## Short term TODOs
1. Write more tests.
2. Document code.
3. Implement the boxed types (float, int32, uint32, Vector, String etc)
4. Implement some looping structure (since recursion is very limited)
5. Implement functions for dealing with references to valus (a reference type?)
6. Be much more stringent on checking of error conditions etc. 
7. Fix the issue with define, that requires a quoted symbol as first argument.

## Long term TODOs
1. Fix problem 1 (below) in one of the many ways possible.

## Problems
1. Stack overflows when doing deep recursion, due to nature of the evaluator. No deep recursion supported.


## Compile for Zynq instructions

1. Build library for ARM A9: `PLATFORM=zynq make`

2. Create a standalone app in Vivado SDK (helloworld template is a good starting point) 

3. Go to menu: Project > Properties > Tool settings > ARM v7 gcc linker > Miscellaneous
   and add in OTHER OBJECTS the path to library.a (build/zynq/library.a)

4. Go to menu: Project > Properties > Tool settings > ARM v7 gcc compiler > Directories
   and add in INCLUDE DIRECTORIES the path to the include directory.

5. Go to menu: Xilinx > Generate linker script
   and set up for enough of heap and stack. 128MB heap and 16MB stack is an ok starting point.

6. Remove the helloworld.c file from the project and replace with repl-zynq/repl.c

7. build and run. 
