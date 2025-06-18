
(define a (list 1 2 3))
(define b (cons 1 a))

(define c (list 4 5 6))
(define d (list 7 8 9))

(define e (list c d))


(defun main ()
  (print "booting image")
  )

(image-save)
(fwrite-image (fopen "s0.lbm" "w"))
