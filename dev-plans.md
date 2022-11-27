
# Half-baked ideas

* Efficient allocation of n heap-cells in one go.
* Function application is a bit clunky.
  Look over and see if it can be streamlined.
* Types that utilize lbm_memory could perhaps be made more uniform and
  maybe then lead to code-reuse?
* Values that are created on the C side could benefit from having an
  associated destructor that is called when the value is GCed.
* Quasiquotation expansion is expensive in heap-cells.
* Quasiquotation expansion Error checking and handling.
* Quasiquotation evaluation does not perform GC if needed.
* Grab general programming extensions from vedderb/bldc.





## Quasiquotation: space inneficient

# `(1 2 ,(+ 1 2))
# (append (quote (1)) (append (quote (2)) (append (list (+ 1 2)) (quote nil))))

When it could expand to:

(append (quote (1 2)) (list (+ 1 2)))

The trade-off here is that expansion will be more compute intensive
and take longer. If the resulting expression is smaller, evaluation will
however be faster. Expansion is done in C and evaluation uses the lbm rts, so
paying a little C time for, what is likely to be much greater, LBM time. 






