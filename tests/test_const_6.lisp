

;; Macros expand at runtime, not readtime.
;; Keep in mind that (defun f (x) ...) creates an f in normal heap,
;; even if that code is in a @const. 

@const-start

(defun f (x y) (+ x y))

(define a '(1 2 3 4))

@const-end

(eq '(2 3 4 5) (map (f 1) a))
