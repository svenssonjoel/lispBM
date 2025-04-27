
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
