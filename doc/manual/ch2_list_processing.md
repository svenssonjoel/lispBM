
# Chapter 2: List processing

This chapter is all about getting familiar with lists. Lists are
of course a very important concept to get quite well familiarised with
as in Lisp we express our code and data using this construct.

In this chapter the programs will start to become slightly larger and
will be written into files and loaded into the REPL. All examples are
available [here](./ch2_examples).


## Getting started with data in lists

Lists in LBM are build from so-called cons-cells with a `car` and a `cdr`
field. The `car` and `cdr` fields are both large enough to hold a pointer
on the architecture we compile for, so 32bits on 32bit architectures and
64bits on 64bit architectures. The LBM heap is a collection of cons-cells
that you can use build up structures such as lists.

The shortest list possible has zero elements and in LBM we use `nil` to
represent this list (`nil` is used for a lot of things in LBM). Longer
lists are constructed from cons-cells that are allocated from the heap.

There are many different ways to build a list in LBM, the most basic way
is to build it manually using `cons` and `nil`. `cons` is a function
that takes two arguments, allocates a cons-cell from the heap and sticks
argument one in the `car` field and argument two in the `cdr`.

Example:
```lisp
(cons 1 nil)
```
Creates a one element list with the value 1 in it.

Example:
```lisp
(cons 1 (cons 2 nil))
```
Creates a two element list with the values 1 and 2.

The expression `(cons 1 (cons 2 nil))` has the same result as the
expression `(list 1 2)` but the first form is more telling about exactly how
lists are constructed and in the case of LBM also how they are represented
on the heap as two cons-cells. There are other expressions that result
in the same list as well, `'(1 2)` and ```(1 2)``.


## Writing functions on lists




## Conclusion
