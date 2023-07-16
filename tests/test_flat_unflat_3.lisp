
(define a (flatten 10 "hej"))

(check (eq (unflatten a) "hej"))
