
@const-start 

(define f (lambda (x) (print "hello " x)))

(define g (lambda (x) (+ x 1)))

(define l (lambda (n)
            {
            (f "apa")
            (print (g n))
            (sleep 0.1)
            (l (+ n 1))
            }))

(define a1 1)
(define a2 2u)
(define a3 3i32)
(define a4 4u32)

(define ls (list 'a1 'a2 'a3 'a4))

@const-end

(define t000 'ext-apa)

(define ls0 (list 1 2 3 ))

@const-start

(define ls1 (cons 100 ls0))

@const-end

(define b1 1)
(define b2 2u)

(define ls2 (cons 75 ls))

(defun main ()
  (print "booting image")
  )

(define my-buffer [1 2 3 4 5 6])

(image-save-dynamic-extensions)
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
