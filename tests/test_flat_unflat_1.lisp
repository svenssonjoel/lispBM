
(define a (flatten 200 '(1 2u32 3i32 3.0)))

(check (eq (unflatten a) '(1 2u32 3i32 3.0)))
