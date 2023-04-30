
(define lib
  (make-env {
   (define a 10)
   (defun f (x) (+ x 1))
   }))

(move-to-flash lib)

(defun f (x) (+ x 100))

(check (and
        (= (in-env lib (f a)) 11)
        (= (f 1) 101)))
