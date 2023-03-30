

(define a0 10)
(define a1 20)

(define a @const `(,a0 . ,a1))

(eq a '(10 . 20))
