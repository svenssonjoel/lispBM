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
6. Two eval functions, one of which evaluate tail-recursive programs to execute in constant(ish) space. 

## Short term TODOs
1. Implement some "reference to X type", for uint32, int32, float perhaps...  
2. Write a small library of useful hofs. 
3. Clean up a lot in eval and eval_cps.  
4. Test the Zynq build more. 
5. Document code.
6. Implement the boxed types ( int32, Vector, String etc).
7. Implement some looping structure (since recursion is very limited).
   Maybe not so limited anymore. Built in loops could be added for speed though.. 
8. Be much more stringent on checking of error conditions etc.

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
