
(define f (lambda (x)
            (if (= x 0) 0
              (f (- x 1)))))


(define apa 0)

(define g (lambda (x) {
            (f x)
            (setq apa 10)
            }
            ))

(define p1 (spawn f 10000))
(define p2 (spawn f 10000))
(define p3 (spawn f 10000))
(define p4 (spawn f 10000))
(define p5 (spawn g 10000))

(wait p5)

(check (= apa 10))
