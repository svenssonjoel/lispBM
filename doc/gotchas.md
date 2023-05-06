De# LBM Gotchas and Caveats

This document collects caveats and gotchas as they are discovered. Hopefully these will
also be given efficient solutions at some points in which case the solution to that gotcha
will also be documented here.

If you find any more gotchas, please let me know how to trigger them! Would be much appreciated.

## Environment

In LBM there is a global environment which is an association list of `(symbol . value)` pairs.
There is also a local environment that is carried along in the evaluator while evaluating
expressions. This local environment is also an association list `(symbol . value)` pairs.

### Closure Gotcha!

When a closure is created, a reference to the local environment is stored in the closure object.
This is for efficiency reasons as traversing the expression and pulling in all free variables
would be a somewhat costly operation (order of the size of the expression) while saving a reference
to the local environment is an O(1) operation. This essentially trades some space (as it potentially
prohibits GC from removing some dead values during the life-time of that closure) for improved performance.

Note that the global environment is not "saved" in the same way inside the closure. Given that
`undefine` exists in LBM, this is a potential foot-gun as illustrated by the example below:

```clj
# (define a 10)
> 10
# (define f (lambda (x) (+ x a)))
> (closure (x) (+ x a) nil)
# (undefine 'a)
> ((f closure (x) (+ x a) nil))
# (f 1)
***	Error:	variable_not_bound
***		a
# 
***	Between rows: (-1 unknown) 
***		Start: -1
***		End:   -1
# 
> variable_not_bound
#
```

Currently no (efficient) solution in mind for this gotcha. Just be careful if you use `undefine`.

### `make-env` Gotcha!

`make-env` is used to create a value representing an environment. This environment value
can be passed around to functions as any other value. To evaluate an expression within the
environment, `in-env` is used. 

As an example we can create an environment with some defined values:

```clj
(define e0 (make-env
            {
            (define a 10)
            (define b 20)
            (define c 30)
            })
  )
```

Now `e0` is the association list `((a . 10) (b . 20) (c . 30))` and we can evaluate an expression
in that environment if we like:

```clj
# (in-env e0 (+ a b c))
> 60
#
```

One can expect that if we put a function inside the environment it should be able to use
the other values defined in that same environment. But that has some caveats!

```clj
(define e1 (make-env
           {
           (define a 10)
           (define f (lambda (x) (+ x a)))
           })
  )
```

The code above defines an environment with a function `f` and a value `a`. `f` tries to use
the value `a` in its body. However:

```clj
# (in-env e1 (f 1))
***	Error:	variable_not_bound
***		a
# 
***	Between rows: (-1 unknown) 
***		Start: -1
***		End:   -1
# 
> variable_not_bound
#
```

This happens because at the time when we define `f`, `a` is in the global
environment and thus not captured into the closure. But when we try to
execute `f` in the env `e1`, `a` is in the local environment which at the
time of evaluation of a function is ignored. The evaluator assumes that
all values needed to evaluate a closure is either inside the closure environment or
passed in as argument or a global.

If we were to also include the local environment in what is visable to the closure, we would
get some kind of dynamic binding which is not desired.

To work around this caveat the environment could be created like this instead:

```clj
(define e2 (make-env
           {
           (define a 10)
           (define f (lambda (x) (in-env e2 (+ x a))))
           })
  )
``` 

Notice the use of `in-env e2` in the body of the function `f`.

Now evaluating `(in-env e2 (f 1))` returns the expected result:

```clj
# (in-env e2 (f 1))
> 11
# 
```




