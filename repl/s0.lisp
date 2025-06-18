
(define a (list 1 2 3))
(define b (cons 1 a))

(define c (list 4 5 6))
(define d (list 7 8 9))

(define e (list c d))

(define barr0 [1 2 3 4])
(define barr_pair (cons barr0 [0 0 0 0]))

(define arr0 [| 1 2 3 |])
(define arr_pair (cons arr0 [| 9 8 7 |]))

(define b1 (cons 2 a))
(define e1 (list c d))

(defun main ()
  (print "booting image")
  )

(image-save)
(fwrite-image (fopen "s0.lbm" "w"))
