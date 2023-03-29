
(define a @const-start '(1 2 3 4 5 6 7 8) @const-end)


(eq (map (lambda (x) (+ x 1)) a) '(2 3 4 5 6 7 8 9))
  
