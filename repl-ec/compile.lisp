
;; Work in progress
;; Experimenting with compilation of lispBM to bytecode
;; while peeking a lot in the SICP book.


;; Will need a way to set this definition to another value
(define compiler-symbols '())

(define all-regs '(env
		   proc
		   val
		   argl
		   cont))

(define all-instrs '(jmpcnt
		     jmpimm
		     movimm
		     lookup
		     setglb
		     push
		     pop
		     mcp
		     ldenv
		     exenv))


(define is-symbol
  (lambda (exp) (= type-symbol (type-of exp))))

(define is-label
  (lambda (exp) (= (car exp) 'label)))

(define is-def
  (lambda (exp) (= (car exp) 'define)))

(define is-nil
  (lambda (exp) (= exp 'nil)))

(define is-quoted
  (lambda (exp) (= (car exp) 'quote)))

(define is-lambda
  (lambda (exp) (= (car exp) 'lambda)))

(define is-list
  (lambda (exp) (= type-list (type-of exp)))

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

(define label-counter 0)

(define incr-label
  (lambda ()
    (progn
      ;; Define used to redefine label-counter
      (define label-counter (+ 1 label-counter))
      label-counter)))

(define mk-label
  (lambda (name)
    (list 'label name (incr-label))))


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

(define regs-needed
  (lambda (s)
    (if (is-label s)
	'()
      (car s))))

(define regs-modified
  (lambda (s)
    (if (is-label s)
	'()
      (car (cdr s)))))

(define statements
  (lambda (s)
    (if (is-label s)
	(list s)
      (car (cdr (cdr s))))))

(define needs-reg
  (lambda (s r)
    (mem r (regs-needed s))))

(define modifies-reg
  (lambda (s r)
    (mem r (regs-modified s))))


(define append-two-instr-seqs
  (lambda (s1 s2)
 	   (mk-instr-seq
	    (list-union (regs-needed s1)
	   		(list-diff (regs-needed s2)
	   			   (regs-modified s1)))
	    (list-union (regs-modified s1)
	   		(regs-modified s2))
	    (append (statements s1) (statements s2))))))
(define append-instr-seqs
  (lambda (seqs)
    (if (is-nil seqs)
	empty-instr-seq
      (append-two-instr-seqs (car seqs)
		  (append-instr-seqs (cdr seqs))))))

(define tack-on-instr-seq
  (lambda (s b)
    (mk-instr-seq (regs-needed s)
		  (regs-modified s)
		  (append (statements s) (statements b)))))
  

(define preserving
  (lambda (regs s1 s2)
    (if (is-nil regs)
	(append-instr-seqs (list s1 s2))
      (let ((first-reg (car regs)))
	(if (and (needs-reg s2 first-reg)
		 (modifies-reg s1 first-reg))
	    (preserving (cdr regs)
	     		(mk-instr-seq
	     		 (list-union (list first-reg)
	     			     (regs-needed s1))
	     		 (list-diff (regs-modified s1)
	     			    (list first-reg))
	     		 (append `((push ,first-reg))
	     			 (append (statements s1) `((pop ,first-reg)))))
	     		s2)

	  (preserving (cdr regs) s1 s2))))))

(define parallel-instr-seqs
  (lambda (s1 s2)
    (mk-instruction-seq
     (list-union (regs-needed s1)
		 (regs-needed s2))
     (list-union (regs-modified s1)
		 (regs-modified s2))
     (append (statements s1) (statements s2)))))
		 

;; COMPILERS

(define mk-instr-seq
  (lambda (needs mods stms)
    (list needs mods stms)))

(define empty-instr-seq (mk-instr-seq '() '() '()))

(define compile-linkage
  (lambda (linkage)
    (if (= linkage 'return)
	;; jmpcnt implies usage of cont register
	(mk-instr-seq '(cont) '() '(jmpcnt))
      (if (= linkage 'next)
	  empty-instr-seq
	(mk-instr-seq '() '() `((jmpimm ,linkage)))))))

(define end-with-linkage
  (lambda (linkage seq)
    (preserving '(cont)
    		seq
    		(compile-linkage linkage))))


(define compile-self-evaluating
  (lambda (exp target linkage)
    (end-with-linkage linkage
		      (mk-instr-seq '()
				    (list target)
				    `((movimm ,target ,exp))))))
(define compile-quoted
  (lambda (exp target linkage)
    (end-with-linkage linkage
		      (mk-instr-seq '()
				    (list target)
				    `((movimm ,target ,(car (cdr exp))))))))
(define compile-symbol
  (lambda (exp target linkage)
    (end-with-linkage linkage
		      ;; Implied that the lookup looks in env register
		      (mk-instr-seq '(env)
				    (list target)
				    `((lookup ,target ,exp))))))
(define compile-def
  (lambda (exp target linkage)
     (let ((var (car (cdr exp)))
	   (get-value-code
	    (compile-instr-list (car (cdr (cdr exp))) 'val 'next)))
       (end-with-linkage linkage
       			 (append-two-instr-seqs get-value-code
						(mk-instr-seq '(val) (list target)
							      `((setglb ,var val)
								(movimm ,target ,var))))))))

(define compile-lambda
  (lambda (exp target linkage)
    (let ((proc-entry    (mk-label "entry"))
	  (after-lambda  (mk-label "after-lambda"))
	  (lambda-linkage (if (= linkage 'next) after-lambda linkage)))
      (append-two-instr-seqs
       (tack-on-instr-seq
	(end-with-linkage lambda-linkage
			  (mk-instr-seq '(env) (list target)
					`((mcp ,target ,proc-entry env))))
	(compile-lambda-body exp proc-entry))
       after-lambda))))

;; TODO: Change ldenv and add another register to hold a heap-ptr
;;       to the formals list. perhaps?
;;       Decide at run-time what to do with mismatch of n-args and n-formals
(define compile-lambda-body
  (lambda (exp proc-entry)   
    (let ((formals (car (cdr exp))))
      (append-two-instr-seqs
       (mk-instr-seq '(env proc argl) '(env)
    		     `(,proc-entry
    		       (ldenv proc)
    		       (exenv ,formals
    			      argl
    			      env)))
       (compile-instr-list (car (cdr (cdr exp))) 'val 'return)))))
	 
(define compile-application
  (lambda (exp target linkage)
    (let ((proc-code (compile-instr-list (car exp) 'proc 'next))
	  (operand-codes
	   (map (lambda (o) (compile-instr-list o 'val 'next)) (car (cdr exp)))))
      (preserving '(env cont)
		  proc-code
		  (preserving '(proc cont)
			      (construct-arglist operand-codes)
			      (compile-proc-call target linkage))))))

(define construct-arglist
  (lambda (codes)
    (let ((operand-codes (reverse codes)))
      (if (is-nil operand-codes)
	  (mk-instr-seq '() '(argl)
			'(movimm argl ()))
	(let ((get-last-arg
	       (append-two-instr-seqs (car operand-codes)
				      (mk-instr-seq '(val) '(argl)
						    '((cons argl val))))))
	  (if (is-nil (cdr operand-codes))
	      get-last-arg
	    (preserving '(env)
			get-last-arg
			(get-rest-args (cdr operant-codes)))))))))

;; (define (code-to-get-rest-args operand-codes)
;;   (let ((code-for-next-arg
;;          (preserving '(argl)
;;           (car operand-codes)
;;           (make-instruction-sequence '(val argl) '(argl)
;;            '((assign argl
;;               (op cons) (reg val) (reg argl)))))))
;;     (if (null? (cdr operand-codes))
;;         code-for-next-arg
;;         (preserving '(env)
;;          code-for-next-arg
;;          (code-to-get-rest-args (cdr operand-codes))))))

;; (define (compile-procedure-call target linkage)
;;   (let ((primitive-branch (make-label 'primitive-branch))
;;         (compiled-branch (make-label 'compiled-branch))
;;         (after-call (make-label 'after-call)))
;;     (let ((compiled-linkage
;;            (if (eq? linkage 'next) after-call linkage)))
;;       (append-instruction-sequences
;;        (make-instruction-sequence '(proc) '()
;;         `((test (op primitive-procedure?) (reg proc))
;;           (branch (label ,primitive-branch))))
;;        (parallel-instruction-sequences
;;         (append-instruction-sequences
;;          compiled-branch
;;          (compile-proc-appl target compiled-linkage))
;;         (append-instruction-sequences
;;          primitive-branch
;;          (end-with-linkage linkage
;;           (make-instruction-sequence '(proc argl)
;;                                      (list target)
;;            `((assign ,target
;;                      (op apply-primitive-procedure)
;;                      (reg proc)
;;                      (reg argl)))))))
;;        after-call))))


(define compile-instr-list
  (lambda (exp target linkage)
    (if (is-self-evaluating exp)
	(compile-self-evaluating exp target linkage)
      (if (is-quoted exp)
	  (compile-quoted exp target linkage)
	(if (is-symbol exp)
	    (compile-symbol exp target linkage)
	  (if (is-def exp)
	      (compile-def exp target linkage)
	    (if (is-lambda exp)
		(compile-lambda exp target linkage)
	      (if (is-list exp)
		  (compile-application exp target linkage)
	  'print "Not recognized"))))))))
