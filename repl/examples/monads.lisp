
(defun concatacc (acc ls)
  (if (eq ls nil) acc
    (concatacc (appenc acc (car ls)) (cdr ls))))

(defun replicate (a n)
  (if (= n 0) nil
    (cons a (replicate a (- n  1)))))

;; List monad

(defun returnlist (a) (list a))
(defun bindlist (la f)
  (map f la))

;; Bunnies example

(defun generation (n)
  (lambda (a) (replicate a n)))

(defun test1 () (bindlist (returnlist "bunny") (generation 5)))

(defun test2 () (bindlist (list "bunny" "rabbit") (generation 5)))

;; a monad object

(defstruct monad (ret bind))

(define listmonad (make-monad))
(setix listmonad 1 returnlist)
(setix listmonad 2 bindlist)

;; Generic monad operations

(defun mret (m a)
  ((ix m 1) a))

(defun >>= (m a f)
  ((ix m 2) a f))
                
;; Bunnies again

(defun test3 () (>>= listmonad (mret listmonad "bunny") (generation 3)))
(defun test4 () (>>= listmonad (list "bunny" "rabbit") (generation 2)))
