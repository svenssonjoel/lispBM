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
4. ... 

## Short term TODOs
1. Write more tests.
2. Document code.
3. Implement the boxed types (float, int32, uint32, Vector, String etc)
4. Implement some looping structure (since recursion is very limited)
5. Implement functions for dealing with references to valus (a reference type?)
6. Be much more stringent on checking of error conditions etc. 
7. Fix the issue with define, that requires a quoted symbol as first argument.

## Long term TODOs
1. fix problem 1 in one of the many ways possible.

## Problems
1. Stack overflows, due to nature of the evaluator. No deep recursion supported.

