(define f (lambda (acc n) (if (num-eq n 0) acc (f (+ acc n) (- n 1)))))

( = (f 0 10000U) 50005000U)
