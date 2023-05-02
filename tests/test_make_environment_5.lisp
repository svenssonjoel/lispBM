
(define lib '( (a . 10) (b . 20)))

(check (= (in-env lib (+ a b) 30)))
