(defun fun (arg) (cons arg (rest-args)))
(defun fun-no-args () (rest-args))

(check (and
    (eq (apply fun '(a)) '(a))
    (eq (apply fun '(a b)) '(a b))
    (eq (apply fun-no-args '()) nil)
    (eq (apply fun-no-args '(a b)) '(a b))
    (eq (apply list '(a b)) '(a b))
    (eq (apply and '(t nil)) nil)
    (= (apply + '(1 2 3)) 6)
))

