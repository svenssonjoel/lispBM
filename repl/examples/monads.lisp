
(defun concatacc (acc ls)
  (if (eq ls nil) acc
    (concatacc (append acc (car ls)) (cdr ls))))

(defun replicate (a n)
  (if (= n 0) nil
    (cons a (replicate a (- n  1)))))

;; List monad

(defun returnlist (a) (list a))
(defun bindlist (la f)
  (concatacc '() (map f la)))

;; Bunnies example

(defun generation (n)
  (lambda (a) (replicate a n)))

(defun test1 () (bindlist (returnlist "bunny") (generation 5)))
(defun test2 () (bindlist (list "bunny" "rabbit") (generation 5)))

;; a monad object

(defstruct monad (ret bind))

(define listmonad (make-monad))
(monad-ret listmonad returnlist)
(monad-bind listmonad bindlist)

;; Generic monad operations

(defun mret (m a)
  ((monad-ret m) a))

(defun >>= (m a f)
  ((monad-bind m) a f)) 
                
;; Bunnies again

(defun test3 () (>>= listmonad (mret listmonad "bunny") (generation 3)))
(defun test4 () (>>= listmonad (list "bunny" "rabbit") (generation 2)))

;; macro
(defmacro do (m body)
  (match body
         ( (((? a) <- (? b)) . (? xs)) 
           `(>>= ,m ,b (lambda (,a) (do ,m ,xs))))
         ( ((? a) . nil) a)
         ( ((? a) . (? xs))
           `(progn ,a (do ,m ,xs)))
         ))

(defun test5 ()
  (do listmonad
    (
     (a <- (list 1 2 3 4))
     (b <- (list 5 6 7 8))
     (mret listmonad (* a b))
     )
    ))

;; Generic monad operation

(defun zip-combos (m f ma mb)
  (do m
      (
       (a <- ma)
       (b <- mb)
       (mret m (f a b)))))

(defun test6 ()
  (zip-combos listmonad (lambda (a b) (* a b)) (list 1 2 3 4) (list 5 6 7 8)))


;; Identity monad

(define idmonad (make-monad))
(monad-ret idmonad (lambda (x) x))
(monad-bind idmonad (lambda (a f) (f a)))

(defun test7 ()
  (zip-combos idmonad (lambda (a b) (* a b)) 2 10))

;; Identity monad vs progn

(defun test8 ()
  (do idmonad
      (
       (print "hello")
       (a <- (+ 1 2))
       (print "the result of (+ 1 2) is " a)
       )))

(defun test9 () ;; PROGN is { } in LBM
  {
  (print "hello")
  (var a (+ 1 2))
  (print "the result of (+ 1 2) is " a)
  }
  )


