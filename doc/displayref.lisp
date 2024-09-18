

(define create_image1
  (ref-entry "img-buffer"
             (list
              (para (list "Allocate an image buffer from lbm memory or from a compactible region."
                          "The form of an `img-buffer` expression is `(img-buffer opt-dm format width height)`."
                          ))
              (code '((define my-img (img-buffer 'indexed2 100 100))
                      ))
              (program '(((define my-dm (dm-create 6000))
                          (define my-img (img-buffer my-dm 'indexed2 100 100))
                          )
                          ))
              end)))


(define manual
  (list
   (section 1 "LispBM Display Reference Manual"
            (list create_image1)
            )
   )
  )

(defun render-manual ()
  (let ((h (fopen "displayref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "Display reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
