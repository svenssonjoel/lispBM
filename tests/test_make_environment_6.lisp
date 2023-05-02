
(define lib
  (make-env {
            (define a 10)
            (define b 20)
            (defun f (x) (+ x a))
   }))


;;(defun f (x) (+ x 100))
lib
;;(in-env lib (f 5))
;;(check (= (in-env lib (f 10)) 20))
