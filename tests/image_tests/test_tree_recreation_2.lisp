
(def tree '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))

(defun main () {
       ;; t3 recreates as
       ;; (((1 2 3) 1 2 3) (1 2 3) 1 2 3)
       ;; which is incorrect.
       (print tree)
       (if (eq tree '(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
           (print "SUCCESS")
           (print "FAILURE")
           )
       })

(print tree)
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
