
# Half-baked ideas

* Efficient allocation of n heap-cells in one go.
* Can function application be made more efficient?
* Types that utilize lbm_memory could perhaps be made more uniform and
  maybe then lead to code-reuse?
* Values that are created on the C side could benefit from having an
  associated destructor that is called when the value is GCed.
* Quasiquotation expansion is expensive in heap-cells.
* Quasiquotation evaluation does not perform GC if needed.
* Grab general programming extensions from vedderb/bldc.


## Allocate N heap cells in one call

Implement by cutting off N cells from the free-list. Not hard but must pay
attention to update counters correctly.

## Function application

There are 5 kinds of applications
 1. Closure
 2. Fundamental operation
 3. Extension
 4. Macro
 5. continuation

In an application form `(a b c d), a is evaluated and then the
continuation APPLICATION_START runs. 

APPLICATION_START differentiates between CLOSURE, MACRO and OTHER form
of application.

if OTHER cont_application_args is immediately called. 

if MACRO, an environment with parameters to unevaluated arguments is created
and the macro body is set to be evaluated in that environment. After evaluating
the macro body, the CONT_R continuation is run.

if CLOSURE, the closure is pushed on the stack and the 1st argument is set
to be evaluated. The CLOSURE_ARGS continuation is set to execute after the argument
is evaluated. 

Conclusion:
There is a lot going on in function application. Some details here and
there can be made more effiecient probably. But as it is now the code
is pretty readable and makes sense.

Some conditionals can be made a bit more efficient here by rearrangements.
but nothing major.

## Types using LBM Memory and destructors

The types that use LBM memory are: arrays, character channels,
custom types and boxed values.

Arrays and character channels are represented by a struct while
custom_types are three lbm_memory words containing value, identifier and
destructor.

* Custom types should have a struct backing them.

Arrays and character channels are often passed to LBM from C.
Should these also have a destructor field?

If all these things have a destructor field a single "destruct" function
could be added to apply the destructor explicitly if it is present. Destruct
should leave the heap-representation of the object in a state that makes sense.

All of the above are represented as '(ptr . identifier-symbol) on the heap.
Here ptr points to some memory holding the struct and the identifier-symbol
informs GC how to deal with the ptr. the cons cell `'(ptr . identifier-symbol)` has
flags set to differentiate it from a list or tuple.

If all these special types were represented by a struct:
```
typedef struct {
        destructor_fptr destructor;
        void *ptr;
}
```
The gc would need no special cases for arrays, channels, custom, etc.
it would just check the flag and see that it is "special" and then invoke
destructor.

if a "special" thing is not passed from C it should have a default destructor.
In the case of an LBM memory array, that destructor is just lbm_memory_free.

Tradeoff here involves extra storage being used for possibly simpler code.
Another benefit is that if memory is being allocated on the C side and passed
to the LBM, it can be automatically freed by GC. That memory may be allocated
for longer than needed though as GC wont run until needed.

Implicit destruction with `destruct` should run the destructor and then
reset the heap representation to `'(nil . nil)` with a normal cons-cell
flag.

It is already pretty expensive to store boxed things in general. Storing
a u32 for example uses up a heap-cell, that's 8 bytes and the 4 bytes
of lbm memory for a total of 12 bytes.

If boxed values could be changed so that they do not use a heap-cell that
would make things a lot better (safer when interoperating with C as well
as the RTS would not need to be paused when creating the heap-cell).

Since heap pointers are indices into the heap array, we always have bits
free at the most significant end of a heap-ptr. We could likewise store
a lbm_memory in a pointer value (bit zero is 1). This would make
mark_phase a bit more complicated though. -> Not really a cell is
identified as a heap-cell pointer by anding with LBM_PTR_MASK yielding non-zero.
with this change we would have to check that x & LBM_PTR_MASK == LBM_PTR_MASK.
so small addition in work there.

CONCLUSION: I think this is worth looking into as it means boxed values
do not need to use a heap-cell. just a pointer-value and an LBM memory word (or more).

About destructors I am not sure yet. Adding them may simplify some code, but it adds
cost again in that a destructor fptr needs to be stored.

There are however many bits to utilize in a ptr-value so we could differentiate
between ptr with destructor and ptr without destructor. but that again means there
will have to be a conditional differentiating between them.

## Quasiquotation: space inneficient

```
`(1 2 ,(+ 1 2))
(append (quote (1)) (append (quote (2)) (append (list (+ 1 2)) (quote nil))))
```

When it could expand to:

(append (quote (1 2)) (list (+ 1 2)))

The trade-off here is that expansion will be more compute intensive
and take longer. If the resulting expression is smaller, evaluation will
however be faster. Expansion is done in C and evaluation uses the lbm rts, so
paying a little C time for, what is likely to be much greater, LBM time. 

This change is probably best to do once GC can run during quotation expansion
as it may produce garbage.






