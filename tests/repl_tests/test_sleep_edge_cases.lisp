
(define r1 (trap (yield)))
(define r2 (trap (yield 1 2 3)))
(define r3 (trap (sleep)))
(define r4 (trap (sleep 1 2 3)))

(if (and (eq r1 '(exit-error type_error))
         (eq r2 '(exit-error type_error))
         (eq r3 '(exit-error type_error))
         (eq r4 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
