
(def t1 (list 1 2 3))
(def t11 (list 4 5 6))
(def t2 (list t1 t11))
(def t3 (list t2 t2))

(defun main () {
       ;; t3 recreates as
       ;; (((1 2 3) 1 2 3) (1 2 3) 1 2 3)
       ;; which is incorrect.
       (print t3)
       (if (eq t3 '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
           (print "SUCCESS")
           (print "FAILURE")
           )
       })

(print t3)
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
