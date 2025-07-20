(define e1 (trap (match)))

(if (eq '(exit-error eval_error) e1)
    (print "SUCCESS")
    (print "FAILURE"))
