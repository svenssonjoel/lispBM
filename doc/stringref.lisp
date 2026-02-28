
(define manual
  (list
   (section 1 "LispBM String Extensions Reference Manual"
            (list
             (para (list "The string extensions provide functions for manipulating strings,"
                         "converting between strings and other types, and formatting output."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "stringref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "String extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
