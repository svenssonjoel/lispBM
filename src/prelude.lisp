
(define reverse
  (lambda (xs)
    (let ((revacc (lambda (acc xs)
		    (if (= nil xs)
			acc
		      (revacc (cons (car xs) acc) (cdr xs))))))
      (revacc nil xs))))


(define iota (lambda (n)
	       (let ((iacc (lambda (acc i n)
			     (if (> i n)
				 acc
			       (iacc (cons (- n i) acc) (+ i 1) n)))))
		 (iacc nil 0 n))))

(define length (lambda (xs)
		 (let ((len (lambda (l xs)
			      (if (= xs nil)
				  l
				(len (+ l 1) (cdr xs))))))
		   (len 0 xs))))

(define take (lambda (n xs)
	       (if (num-eq n 0)
		   nil
		 (cons (car xs)
		       (take (- n 1) (cdr xs))))))

(define drop (lambda (n xs)
	       (if (num-eq n 0)
		   xs
		 (if (= xs nil)
		     nil
		   (drop (- n 1) (cdr xs))))))

(define zip (lambda (xs ys)
	      (if ( = xs nil)
		  nil
		(if ( = ys nil)
		    nil
		  (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))))

(define map (lambda (f xs)
	      (if (= xs nil)
		  nil
		(cons (f (car xs)) (map f (cdr xs))))))

(define lookup (lambda (x xs)
		 (if (= xs nil)
		     nil
		   (if (= (car (car xs)) x)
		       (cdr (car xs))
		     (lookup x (cdr xs))))))


(define sum (lambda (acc n) (if (num-eq n 0) acc (sum (+ acc n) (- n 1)))))
