(defun list ()
    (cons 'prefix (rest-args))
)

; This is fun ;)
(def t 'nil)
(def nil 't)

(check (and
    (eq (list 1 2 3) '(prefix 1 2 3))
    (eq ('list 1 2 3) '(1 2 3))
    (eq (if t 'then 'else) 'else)
    (eq (if nil 'then 'else) 'then)
))
