
(define tree '(("hello" "kurt") ("russel" "rules")))

(define a (flatten 1000 tree))

(check (eq (unflatten a) tree))
