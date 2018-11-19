# lispBM

A lisp-like language (work in progress) implemented in C using the MPC library for the parsing task.

## Purpose
1. Have fun.
2. Learn about lisp.
3. ...

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. A few built-in functions (define, cons, car, cdr, eval, +, -, >, <, =)
3. Lambdas, closures, lets (letrecs).
4. ... 

## Short term TODOs
1. Write more tests.
2. insert relevant part of local environment into closures in a better way
3. simple_print crashes because of current state of point 2. (If in a let: closure has a ref to local env, which has a ref to closure... repeat forever.) 
3. Document code.
4. ...

## Problems

1. Stack overflows, due to nature of the evaluator. No deep recursion supported. 

