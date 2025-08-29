

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define time-read-eval
    (macro (str)
           `(progn
              (var t0 (systime))
              (var res (read-eval-program ,str))
              (print (- (systime) t0))
              res)))




;; Test 1
(define r1 (= 7 (time-read-eval "(+ 1 2) (+ 3 4)")))

(debug_test r1 1)

;; Test 2
(define r2 (eq 'exit-error (car (trap (time-read-eval "(undefined-function 42)")))))

(debug_test r2 2)

;; Test 3
(define r3 (eq 'exit-error (car (trap (time-read-eval "(unclosed paren")))))

(debug_test r3 3)

;; Test 4
(define r4 (eq nil (time-read-eval "")))

(debug_test r4 4)

;; Test 5
(define r5 (eq 'exit-error (car (trap (time-read-eval "(/ 1 0)")))))

(debug_test r5 5)

;; Test 6
(define r6 (= 42 (time-read-eval "(define test-var 42) test-var")))

(debug_test r6 6)

;; Test 7 - Larger program with recursion
(define r7 (= 120 (time-read-eval "
  (define fact (lambda (n)
    (if (< n 2)
        1
        (* n (fact (- n 1))))))
  (fact 5)")))

(debug_test r7 7)

;; Test 8 - Program with loops and list operations
(define r8 (= 5050 (time-read-eval "
  (define sum 0)
  (looprange i 1 101
    (setq sum (+ sum i)))
  sum")))

(debug_test r8 8)

;; Test 9 - Program with nested data structures
(define r9 (= 6 (time-read-eval "
  (define nested '((1 2) (3 4) (5 6)))
  (define process (lambda (lst)
    (if (eq lst nil)
        0
        (+ (length (car lst)) (process (cdr lst))))))
  (process nested)")))

(debug_test r9 9)

;; Test 10 - Program with string operations
(define r10 (eq "HELLO WORLD" (time-read-eval "
  (define msg \"hello world\")
  (str-to-upper msg)")))

(debug_test r10 10)

;; Test 11 - Long symbol from file (should trigger tokenizer error)
(define file11 (fopen "repl_tests/test_data/long_symbol.lisp" "r"))
(define long-program (load-file file11))
(fclose file11)
(define r11 (eq 'exit-error (car (trap (time-read-eval long-program)))))

(debug_test r11 11)

;; Test 12 - Unterminated string from file
(define file12 (fopen "repl_tests/test_data/unterminated_string.lisp" "r"))
(define unterminated-program (load-file file12))
(fclose file12)
(define r12 (eq 'exit-error (car (trap (time-read-eval unterminated-program)))))

(debug_test r12 12)

;; Test 13 - Invalid escape sequences from file  
(define file13 (fopen "repl_tests/test_data/invalid_escape.lisp" "r"))
(define invalid-escape-program (load-file file13))
(fclose file13)
(define r13 (eq 'exit-error (car (trap (time-read-eval invalid-escape-program)))))

(debug_test r13 13)

;; Test 14 - Deeply nested expressions from file
(define file14 (fopen "repl_tests/test_data/deeply_nested.lisp" "r"))
(define nested-program (load-file file14))
(fclose file14)
(define r14 (eq 'exit-error (car (trap  (time-read-eval nested-program)))))

(debug_test r14 14)

;; Test 15-23 - Programs with incrementally different byte sizes
;; Tests buffer boundary conditions and alignment issues

(define file15 (fopen "repl_tests/test_data/program_1000.lisp" "r"))
(define prog15 (load-file file15))
(fclose file15)
(define r15 (number? (time-read-eval prog15)))

(debug_test r15 15)

(define file16 (fopen "repl_tests/test_data/program_odd_1.lisp" "r"))
(define prog16 (load-file file16))
(fclose file16)
(define r16 (eq 'exit-error (car (trap  (time-read-eval prog16)))))
;(define r16 (eq 'exit-error (car (trap  (read-eval-program prog16)))))

(debug_test r16 16)

(define file17 (fopen "repl_tests/test_data/program_odd_2.lisp" "r"))
(define prog17 (load-file file17))
(fclose file17)
(define r17 (eq 'exit-error (car (trap  (time-read-eval prog17)))))

(debug_test r17 17)

(define file18 (fopen "repl_tests/test_data/program_1001.lisp" "r"))
(define prog18 (load-file file18))
(fclose file18)
(define r18 (= 120 (time-read-eval prog18)))

(debug_test r18 18)

(define file19 (fopen "repl_tests/test_data/program_odd_3.lisp" "r"))
(define prog19 (load-file file19))
(fclose file19)
(define r19 (eq 'exit-error (car (trap (time-read-eval prog19)))))

(debug_test r19 19)

(define file20 (fopen "repl_tests/test_data/program_1002.lisp" "r"))
(define prog20 (load-file file20))
(fclose file20)
(define r20 (eq exit-error (car (trap (time-read-eval prog20)))))

(debug_test r20 20)

(define file21 (fopen "repl_tests/test_data/program_odd_4.lisp" "r"))
(define prog21 (load-file file21))
(fclose file21)
(define r21 (eq 'exit-error  (car (trap (time-read-eval prog21)))))

(debug_test r21 21)

(define file22 (fopen "repl_tests/test_data/program_1003.lisp" "r"))
(define prog22 (load-file file22))
(fclose file22)
(define r22 (eq 'exit-error (car (trap  (time-read-eval prog22)))))

(debug_test r22 22)

(define file23 (fopen "repl_tests/test_data/program_odd_5.lisp" "r"))
(define prog23 (load-file file23))
(fclose file23)
(define r23 (eq 'exit-error (car (trap  (time-read-eval prog23)))))

(debug_test r23 23)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23)
    (print "SUCCESS")
    (print "FAILURE"))
