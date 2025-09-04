(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))


(defmacro add-em (x y z xs)
  `(+ ,x ,y ,z ,@xs))

;; Test 1
(define r1 (= 55 (add-em 1 2 3 (4 5 6 7 8 9 10))))

(debug_test r1 1)

;;  Test 2
(define r2 t)
(looprange i 0 1000
      (setq r2 (and r2 (= 55 (add-em 1 2 3 (4 5 6 7 8 9 10))))))

(debug_test r2 2)

;;  Test 3

(define space-waste (range 900)) ;; increase likelyhood of GC

(define r3 t)
(looprange i 0 1000
      (setq r3 (and r2 (= 55 (add-em 1 2 3 (4 5 6 7 8 9 10))))))

(debug_test r3 3)

(undefine 'space-waste)

;; Test 4 Strangely formulated macro

(defmacro prod-em (ys xs)
  (cons * `(,@ys ,@xs)))

(define r4 (= 48 (prod-em (1 1 1 1 1 2) (2 3 4))))
(debug_test r4 4)

;; Test 5 Strangely formulated macro stress
(define r5 t)
(looprange i 0 1000
      (setq r5 (and r5 (= 48 (prod-em (1 1 1 1 1 2) (2 3 4))))))

(debug_test r5 5)

;; Test 6 - Buggy do macro with GC safety issue (like the cons/append pattern)
(defstruct monad (ret bind))
(define idmonad (make-monad))
(monad-ret idmonad (lambda (x) x))
(monad-bind idmonad (lambda (a f) (f a)))

(defun mret (m a) ((monad-ret m) a))
(defun >>= (m a f) ((monad-bind m) a f))

(defmacro do (m)
  (match (rest-args)
         ( (((? a) <- (? b)) . (? xs)) 
           `(>>= ,m ,b (lambda (,a)  (do ,m  ,@xs))))
         ( ((? a) . nil) a)
         ( ((? a) . (? xs))
           `(>>= ,m ,a (lambda (_) (do ,m ,@xs))))
         ))

(define r6 (= 42 (do idmonad (x <- 40) (mret idmonad (+ x 2)))))
(debug_test r6 6)

;; Test 7 - Stress test the do macro
(define r7 t)
(looprange i 0 1000
     (setq r7 (and r7 (= 42 (do idmonad (x <- 40) (mret idmonad (+ x 2)))))))

(debug_test r7 7)

(if (and r1 r2 r3 r4 r5 r6 r7)
    (print "SUCCESS")
    (print "FAILURE"))
