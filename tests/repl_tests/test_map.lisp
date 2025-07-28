
(define r1 (trap (map (lambda (x) (+ x 1)) 'apa)))
(define r2 (trap (map (lambda (x) (+ x 1)) [| 1 2 3 |])))
(define r3 (trap (map)))

(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE")
    )
