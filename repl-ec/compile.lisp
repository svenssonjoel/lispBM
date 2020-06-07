
;; Work in progress
;; Experimenting with compilation of lispBM to bytecode
;; while peeking a lot in the SICP book.


;; Will need a way to set this definition to another value
(define compiler-symbols '())

(define is-symbol
  (lambda (exp) (= type-symbol (type-of exp))))

(define is-nil
  (lambda (exp) (= exp 'nil)))

(define is-number
  (lambda (exp)
    (let ((typ (type-of exp))) 
      (or (= typ type-i28)
	  (= typ type-u28)
	  (= typ type-i32)
	  (= typ type-u32)
	  (= typ type-float)))))

(define is-array
  (lambda (exp)
    (= (type-of exp) type-array)))

(define is-self-evaluating
  (lambda (exp)
    (or (is-number exp)
	(is-array  exp))))

(define mem
  (lambda (x xs)
    (if (is-nil xs)
	'nil
      (if (= x (car xs))
	  't
	(mem x (cdr xs))))))

(define list-union
   (lambda (s1 s2)
     (if (is-nil s1) s2
       (if (mem (car s1) s2)
	   (list-union (cdr s1) s2)
	 (cons (car s1) (list-union (cdr s1) s2))))))

(define list-diff
  (lambda (s1 s2)
    (if (is-nil s1) '()
      (if (mem (car s1) s2) (list-diff (cdr s1) s2)
	(cons (car s1) (list-diff (cdr s1) s2))))))


;;(define (make-instruction-sequence needs modifies statements)
;;  (list needs modifies statements))

(define mk-instr-seq
  (lambda (needs mods stms)
    (list needs mods stms)))

(define empty-instr-seq (mk-instr-seq '() '() '()))

(define compile-instr-list
  (lambda (exp target linkage)
    (if (is-self-evaluating exp) 'yes 'no)))
