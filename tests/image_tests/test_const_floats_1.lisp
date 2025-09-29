
(def r 0)

(def my-f1 3.14)
(def my-fl1 (list 3.14 6.28 100.0))

@const-start

(define feq (lambda (a b epsilon)
              (< (abs (- a b)) epsilon)))

(def my-f2 3.14)
(def my-fl2 (list 3.14 6.28 100.0))

(defun f () {
  (looprange i 0 100 {
        (print i)
        (setq r i)
        })
  r
  })

(defun main ()
  (if (and (= (f) 99)
           (feq my-f1 3.14 0.001)
           (feq my-f2 3.14 0.001)
           (feq (ix my-fl1 2) 100.0 0.001)
           (feq (ix my-fl2 2) 100.0 0.001))
           
      
      (print "SUCCESS")
      (print "FAILURE")
      ))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
