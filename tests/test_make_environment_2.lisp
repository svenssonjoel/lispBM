
(define lib
  (make-env {
   (define a 10)
   (defun f (x) (+ x 1))
   }))


(defun f (x) (+ x 100))

(check (= (in-env lib (f a)) 11))
