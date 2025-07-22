; Runtime extension edge cases test

; Basic function tests
(define test1 (number? (mem-num-free)))
(define test2 (list? (lbm-version)))
(define test3 (eq (set-eval-quota 1000) t))
(define test4 (eq (trap (set-eval-quota)) '(exit-error eval_error)))

; lbm-heap-state with valid symbol argument
(define heap_test1 (number? (lbm-heap-state 'get-heap-size)))
(define heap_test2 (number? (lbm-heap-state 'get-heap-bytes)))

; lbm-heap-state with 0 arguments
(define heap_test3 (eq (trap (lbm-heap-state)) '(exit-error type_error)))

; lbm-heap-state with more than 1 argument  
(define heap_test4 (eq (trap (lbm-heap-state 'get-heap-size 'extra)) '(exit-error type_error)))
(define heap_test5 (eq (trap (lbm-heap-state 'get-heap-size 42 "string")) '(exit-error type_error)))

; lbm-heap-state with non-symbol argument
(define heap_test6 (eq (trap (lbm-heap-state 42)) '(exit-error type_error)))
(define heap_test7 (eq (trap (lbm-heap-state "string")) '(exit-error type_error)))

; lbm-heap-state with invalid symbol (should return nil, not error)
(define heap_test8 (eq (lbm-heap-state 'invalid-symbol) nil))

(if (and test1 test2 test3 test4 
         heap_test1 heap_test2 heap_test3 heap_test4 heap_test5 
         heap_test6 heap_test7 heap_test8)
    (print "SUCCESS")
    (print "FAILURE"))