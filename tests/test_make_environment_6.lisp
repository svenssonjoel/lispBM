
(define lib
  (make-env {
            (defun f (x) (+ x 1))
            (define a 10)
            (define b 20)
   }))


(check (and (eq (assoc lib 'a) 10)
            (eq (assoc lib 'b) 20)))


