
(define f @const (closure (x y) (+ x y) nil))

(define a @const '(1 2 3 4))

(eq '(2 3 4 5) (map (f 1) a))
