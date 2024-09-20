
(define evaluation-quota
  (ref-entry "set-eval-quota"
             (list
              (para (list "`set-eval-quota` sets the number of evaluation steps that is"
                          "given to each context when given turn to execute by the round-robin"
                          "scheduler."
                          ))
              (code '((set-eval-quota 30)
                      ))
              

              end)))

              
             
(define manual
  (list
   (section 1 "LispBM Runtime Extensions Reference Manual"
            (list evaluation-quota)
            )
   )
  )

(defun render-manual ()
  (let ((h (fopen "runtimeref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "Runtime reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
