
# KNOWN BUGS


## struct_eq

   struct_eq is an internal function used for comparison of arbitrary
   lisp values. It is for example used in pattern matching. struct_eq
   also implements the `(eq a b)` lisp function.
   
   struct_eq is a recursive C function that can exhaust call stack and
   crash.

   Work around: Do not use `eq` on cyclical or large values.

## flatten

   `flatten` as used in `(flatten a)` from lisp does not detect
   sharing and instead duplicates shared nodes in the flattened representation
   of the value.

   `flatten` is implemented as recursion on the C side and protects
   against stack exhaustion via a recusion counter.

   Note that the flattening that happens when Lisp Images are created
   does not have this shortcoming. The image-flattener uses pointer-reversal
   and thus only O(1) extra memory.

   Work around: Just be aware that `flatten` looses sharing structure.

## match

   `match` implements pattern matching on lisp expressions.
   The implementation of `match` is a recursive C function that
   could exhaust stack given large enough expressions to match on.

   Work around: Be aware that it is not possible to pattern match on
   very deep/large structures. 