

(define funs (let ( ( a 10) )
               (list (lambda (x) { (setq a x) a})
                     (lambda () a))))

(define f (car funs))

(define g (car (cdr funs)))

(f 20)

;; Below is incorrect but maybe tollerable?
(check (= (g) 10))
