(check (and
    (eq (call-cc (fn (return) (apply return '(a)))) 'a)
    (eq (call-cc (fn (return) (apply return '()))) nil)
    (eq (call-cc-unsafe (fn (return) (apply return '(a)))) 'a)
    (eq (call-cc-unsafe (fn (return) (apply return '()))) nil)
))