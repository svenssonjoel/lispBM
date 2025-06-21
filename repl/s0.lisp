
@const-start

(define ca (list 6 6 6))
(define cb (list 9 9 9))

@const-end

(define sc (list ca cb))

(define a (list 1 2 3))
(define b (cons 1 a))

(define c (list 4 5 6))
(define d (list 7 8 9))

(define e (list c d))

;; (define barr0 [1 2 3 4])
;; (define barr_pair (cons barr0 [0 0 0 0]))

;; (define arr0 [| 1 2 3 |])
;; (define arr_pair (cons arr0 [| 9 8 7 |]))

;; (define b1 (cons 2 a))
;; (define e1 (list c d))

;; boxed values are shared.
(define f0 3.14)
(define f_pair (cons f0 f0))
(define f_pair2 (cons 3.14 3.14))

;; non-boxed values are not shared.
(define i0 23)
(define i_pair (cons i0 i0))

(defun main ()
  (print "booting image")
  )

(image-save)
(fwrite-image (fopen "s0.lbm" "w"))
