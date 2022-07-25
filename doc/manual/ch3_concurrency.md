
# Chapter 3: Concurrency

LispBM (LBM) supports concurrently executing
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


## Getting started with concurrent programming in LBM

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

This program is stored in the file [hello.lisp](ch3_examples/hello.lisp)
and you can load it into the REPL if you launched the REPL from the same directory as:

```
# :load hello.lisp
filename: hello.lisp
> hello
# 
```

And now we can run the `hello` function by typing `(hello)` and
hitting return:

```
# (hello)
hello
> t
``` 

That it took 10 seconds for the string "hello" to appear is hard to
show off here ;).


We can make the hello program call itself recursively to repeatedly print hello
every 10 seconds:

```lisp
(defun hello ()
  (progn
    (sleep 10)
    (print "hello")
    (hello)))
```

Running this program will look something like:
```
# :load hello_repeater.lisp
filename: hello_repeater.lisp
> hello
# (hello)
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
hello
...
```

While the hello program is running, you can still interact with the REPL.
For example we can squeeze in an evaluation of `(+ 1 2)`:

```
hello
hello
hello
# (+ 1 2)
> 3
hello
hello
``` 

When we start evaluation of some expression, such as `(hello)` or `(+ 1 2)`,
from the REPL an evaluation context for that expression is created
in the runtime system. An evaluation context is a datastructure maintaining
the evaluation stack and other important data related to the evaluation
of an expression. So, everything launched from the REPL is in essense "spawned"
as a separate process running concurrently with anything else. 

Processes can also be spawned from within programs using the `spawn` operation.

The following example can be found in [hello_bye.lisp](ch3_examples/hello_bye.lisp).

```lisp
(defun hello ()
  (progn
    (sleep 10)
    (print "hello")
    (hello)))


(defun bye ()
  (progn
    (sleep 5)
    (print "bye")
    (bye)))

(spawn hello)
(bye)
``` 

The example above spawns a hello process that says "hello" every 10 seconds
and then it runs the function `bye` which says "bye" every 5 seconds.

```
# :load hello_bye.lisp 
filename: hello_bye.lisp
bye
hello
bye
bye
hello
bye
bye
hello
bye
bye
hello
bye
``` 

When loading a file into the REPL, everything is evaluated from the
top towards the bottom in an evaluation context. When `(spawn hello)`
is encountered, a new context is created for the evaluation of `hello`
and it is placed on a queue of runnable contexts. After putting the `hello`-context on
the runnable queue, the `bye` function is called. The `bye` function will now
run indefinitely in the original context.

## Context queues

The LBM runtime system maintains queues of evaluation contexts. You can list
the contexts from the repl using the command `:ctxs`.


If the `:ctxs` command is executed while the hello_bye.lisp program is
running you get some feedback as below: 

```
hello
bye
bye
# :ctxs
****** Running contexts ******
--------------------------------
ContextID: 475
Stack SP: 4
Stack SP max: 15
Value: t
--------------------------------
ContextID: 145
Stack SP: 4
Stack SP max: 47
Value: t
****** Blocked contexts ******
****** Done contexts ******
``` 

There are two context in the queue of runnable (running) contexts. This queue
holds all contexts that are waiting to be executed and they end up
on this list when they are started or when they evaluated a `yield` (sleep).

There is a queue of blocked contexts that processes will end up on
if they are blocked waiting for messages (see Message passing).
Finally there is a done queue for contexts that have finished evaluating.
The REPL actually removes contexts from the done queue as soon as it has
printed the final result of that context to the user. 

The context that really, currently, is running at the exact same time as
the `:ctxs` command is issued is not pressent on any queue and thus not listed
by the `:ctxs` command.

The information printed by the `:ctxs` commands consists of a ContextID which
is assigned to a context upon creation (this value is also the result returned
by `spawn` when first spawning the process). Next is the current stack pointer
of the context and the maximum stack pointer value ever occured. The "Value" refers
to the last value computed by that context. If the context is in the running queue,
the `Value` should be `t` for true, the return value of `yield` when successful. 

## Message passing



