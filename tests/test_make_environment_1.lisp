
(define lib
  (make-env {
   (define a 10)
   (define b 20)
   (define c 30)
   }))


(check (= (in-env lib (+ a b)) 30))


