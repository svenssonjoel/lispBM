
# Chapter 3: Concurrency LispBM (LBM) supports concurrently executing
processes and let's processes communicate with eachother via message
passing. Currently the runtime system that execute LBM programs is
entirely sequential and does not make use of multiple cores, if
present. Concurrency abstractions are very useful even with there
being any actual parallelism. Each operation performed by an LBM
process can be considered atomic in relationship to any other LBM
process.

One use-case is to run LBM in parallel with some larger embedded
application written in C. The operations performed by LBM processes
are not atomic in relation to operations performed on the C side. This
is only a problem if the C program is accessing data that is stored in
the LBM runtime system (such as on the LBM heap, stack or process
queues). This is something the person integrating LBM into a system
must consider when writing the interface between the C application and
the LBM runtime system. Extensions to LBM that are written in C are
executed synchronously by the runtime system and pose no
trouble. There is however the possibility to implement asynchronous
LBM extensions in C as well, and then one needs to be quite
careful. All of these more advanced use-cases will be explained in a
future chapter.

LBM implements so-called cooperative concurrency which means that
processes run uninterrupted until the time that the process itself
executes an operation that yields usage of the RTS (such as the
`yield` operation). Another example of an operation that yeilds is the
message passing `recv` (receive) operation.  No LBM process will ever
preempt any other LBM process.


## Getting started with concurrent LBM

The `yield` function is used in a process to give up the runtime system
and free it up for doing other work. `yield` takes one argument which is the
number of microseconds for which the process wishes to sleep.

We can define a `sleep` function that puts a process to sleep for a number of
seconds instead.

```lisp
(defun sleep (x)
  (yield (* x 1000000)))
``` 

You can write a function that prints "hello" ten seconds from now as:

```lisp
(defun hello ()
  (progn
    (sleep 10)
    (print "hello")))
``` 

This program is stored in the file [hello.lisp](../ch3_examples/hello.lisp)
and you can load it into the repl if you launched the repl from the same directory as:

```
# :load hello.lisp
filename: hello.lisp
> hello
# 
```

And now we can run the `hello` function by typing `(hello)` and hitting return:

```
# (hello)
hello
> t
``` 

That it took 10 seconds for the string "hello" to appear is hard to show off here ;).











## Message passing



