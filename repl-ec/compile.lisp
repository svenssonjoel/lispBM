
;; Work in progress
;; Experimenting with compilation of lispBM to bytecode
;; while peeking a lot in the SICP book.


;; Will need a way to set this definition to another value
(define compiler-symbols '())

(define is-symbol
  (lambda (exp) (= type-symbol (type-of exp))))

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

(define compile-instr-list
  (lambda (exp target linkage)
    (if (is-self-evaluating exp) 'yes 'no)))
