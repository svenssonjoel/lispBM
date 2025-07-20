
(hide-trapped-error)

(define e (list '(a . 10) '(b . 20) '(c . 30)))

(define e1 (env-drop 'a e))
(define e2 (env-drop 'b e))
(define e3 (env-drop 'c e))

(define err1 (trap (env-drop)))
(define err2 (trap (env-drop 1)))
(define err3 (trap (env-drop 1 e)))
(define err4 (trap (env-drop 'a e 10)))

(if (and (= (assoc e1 'b) 20)
         (= (assoc e2 'a) 10)
         (= (assoc e3 'a) 10)
         (eq '(exit-error type_error) err1)
         (eq '(exit-error type_error) err2)
         (eq '(exit-error type_error) err3)
         (eq '(exit-error type_error) err4))
    (print "SUCCESS")
    (print "FAILURE")
    )
