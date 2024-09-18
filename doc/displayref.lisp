


(display-to-image)
(define render-target (img-buffer 'rgb888 320 200))
(set-active-image render-target)
(disp-clear)

(define create_image1
  (ref-entry "img-buffer"
             (list
              (para (list "Allocate an image buffer from lbm memory or from a compactible region."
                          "The form of an `img-buffer` expression is `(img-buffer opt-dm format width height)`."
                          ))
              (code '((define my-img (img-buffer 'indexed2 320 200))
                      ))
              (program '(((define my-dm (dm-create 10000))
                          (define my-img (img-buffer my-dm 'indexed2 320 200))
                          )
                         ))
              end)))

(define arcs
    (ref-entry "arcs"
	       (list
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-arc my-img 100 100 50 160 100 1)
			    (img-arc my-img 100 100 50 160 100 1 '(dotted 15 15))
			    (img-arc my-img 100 100 50 160 100 1 '(filled))
			    (img-arc my-img 100 100 50 160 100 1 '(thickness 10))
			    (img-arc my-img 100 100 50 160 100 1 '(rounded))
			    ))
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-arc my-img 100 100 50 160 100 1 '(dotted 15 15) '(resolution 3))
			    (img-arc my-img 100 100 50 160 100 1 '(thickness 10) '(rounded))
			  ))
		end)))

(define lines
    (ref-entry "lines"
	       (list
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-line my-img 0 0 320 200 1)
                            (img-line my-img 0 200 320 0 1 '(dotted 4 20))
			    ))
		end)))
	       


(define manual
  (list
   (section 1 "LispBM Display Reference Manual"
            (list create_image1
		  arcs
		  lines)
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
