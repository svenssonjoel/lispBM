
(define reverse
  (lambda (xs)
    (let ((revacc (lambda (acc xs)
		    (if (= nil xs) acc (revacc (cons (car xs) acc) (cdr xs))))))
      (revacc nil xs))))
