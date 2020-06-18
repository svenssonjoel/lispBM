
;; Work in progress
;; Experimenting with compilation of lispBM to bytecode
;; while peeking a lot in the SICP book.


;; Possible Optimisations
;; - If it is known at compile time that a function call is to a
;;   fundamental operation, more efficient call code can be generated.
;;

;; Hold a list of symbols that are used within the compiled code 
(define compiler-symbols '())

(define all-regs '(env
		   proc
		   val
		   argl
		   cont))

(define all-instrs '(jmpcnt
		     jmpimm
		     jmp
		     movimm
		     lookup
		     setglb
		     push
		     pop
		     bpf
		     exenv
		     cons
		     consimm
		     cdr
		     cadr
		     caddr
		     car
 		     callf))

(define instr-size
  '((jmpcnt  1)
    (jmpimm  5) ;; 5 bytes is overkill
    (jmp     2)
    (movimm  5)
    (mov     3)
    (lookup  6) 
    (setglb  9)
    (push    2)
    (pop     2)
    (bpf     5) ;; 5 bytes is overkill
    (exenv   9)
    (cons    3)
    (consimm 6)
    (cdr     3)
    (cadr    3)
    (caddr   3)
    (car     2)
    (callf   1)
    (label   0)))


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

(define is-let
  (lambda (exp) (= (car exp) 'let)))

(define is-progn
  (lambda (exp) (= (car exp) 'progn)))

(define is-list
  (lambda (exp) (= type-list (type-of exp))))

(define is-last-element
  (lambda (exp) (and (is-list exp) (is-nil (cdr exp)))))

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
    (mk-instr-seq
     (list-union (regs-needed s1)
		 (regs-needed s2))
     (list-union (regs-modified s1)
		 (regs-modified s2))
     (append (statements s1) (statements s2)))))


(define instr-seq-bytes
  (lambda (s)
    (let ((sum-bytes
	   (lambda (x acc)
	     (if (is-nil x) acc
	       (sum-bytes (cdr x) (+ (lookup (car (car x)) instr-size) acc))))))
      (sum-bytes (car (cdr (cdr s))) 0))))
	       
;; COMPILERS

(define mk-instr-seq
  (lambda (needs mods stms)
    (list needs mods stms)))

(define empty-instr-seq (mk-instr-seq '() '() '()))

(define compile-linkage
  (lambda (linkage)
    (if (= linkage 'return)
	;; jmpcnt implies usage of cont register
	(mk-instr-seq '(cont) '() '((jmpcnt)))
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

;; TODO: The output code in this case should be code that recreates 
;;       the quoted expression on the heap. At least this would be required
;;       if loading the compiled bytecode into a fresh RTS.
(define compile-quoted
  (lambda (exp target linkage)
    (end-with-linkage linkage
		      (compile-data (car (cdr exp)) target))))

(define compile-data
  (lambda (exp target)
    (if (is-list exp)
	(append-two-instr-seqs
	 (mk-instr-seq '() (list target)
		       `((movimm ,target nil)))
	 (compile-data-list (reverse exp) target))
      (compile-data-prim exp target))))

(define compile-data-nest
  (lambda (exp target)
    (if (is-list exp)
	(append-two-instr-seqs
	 (mk-instr-seq (list target) '()
		       `((push ,target)
			 (movimm ,target nil)))
	 (append-two-instr-seqs
	  (compile-data-list (reverse exp) target)
	  (mk-instr-seq (list target) (list target 'argl)
			`((mov argl ,target)
			  (pop ,target)
			  (cons ,target argl)))))
      (append-two-instr-seqs
       (compile-data-prim exp 'argl)
       (mk-instr-seq '(argl) (list target)
		     `((cons ,target argl)))))))

(define compile-data-prim
  (lambda (exp target)
    (mk-instr-seq '() (list target)
		  `((movimm ,target ,exp)))))

(define compile-data-list
  (lambda (exp target)
    (if (is-nil exp)
	empty-instr-seq
      (append-instr-seqs
       (map (lambda (e)
	      (compile-data-nest e target))
	    exp)))))

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


;; Very unsure of how to do this one correctly.
(define compile-let
  (lambda (exp target linkage)
    (append-two-instr-seqs
     (append-instr-seqs (map compile-binding (car (cdr exp))))
     (compile-instr-list (car (cdr (cdr exp))) target linkage))))

(define compile-binding
  (lambda (keyval)
    (let ((get-value-code
    	   (compile-instr-list (car (cdr keyval)) 'val 'next))
	  (var (car keyval)))
      (append-two-instr-seqs get-value-code			     
      			     (mk-instr-seq '(val) (list 'env)
      					   `((extenv ,var val)))))))
		    
		  
(define compile-progn
  (lambda (exp target linkage)
    (if (is-last-element exp)
	(compile-instr-list (car exp) target linkage)
      (preserving '(env continue)
		  (compile-instr-list (car exp) target 'next)
		  (compile-progn (cdr exp) target linkage)))))
    
(define compile-lambda
  (lambda (exp target linkage)
    (let ((proc-entry    (mk-label "entry"))
	  (after-lambda  (mk-label "after-lambda"))
	  (lambda-linkage (if (= linkage 'next) after-lambda linkage)))
      (append-two-instr-seqs
       (tack-on-instr-seq
	(end-with-linkage lambda-linkage
			  (mk-instr-seq '(env) (list target)
					`((movimm ,target nil)
					  (cons ,target env)
					  (consimm ,target ,proc-entry)
					  (consimm ,target proc)))) ;; put symbol proc first in list
	(compile-lambda-body exp proc-entry))
       after-lambda))))

;; TODO: Change ldenv and add another register to hold a heap-ptr
;;       to the formals list. perhaps?
;;       Decide at run-time what to do with mismatch of n-args and n-formals
(define compile-lambda-body
  (lambda (exp proc-entry)   
    (let ((formals (car (cdr exp))))
      (append-two-instr-seqs
       (append-two-instr-seqs 
	(mk-instr-seq '(env proc argl) '(env)
		      `(,proc-entry
			(caddr env proc)))
	(append-instr-seqs
	 (map (lambda (p)
		(mk-instr-seq '(argl) '(env)
			      `((exenv ,p argl env))))
	      formals)))
       (compile-instr-list (car (cdr (cdr exp))) 'val 'return)))))
	 
(define compile-application
  (lambda (exp target linkage)
    (let ((proc-code
	   (if (is-fundamental (car exp))
	       (mk-instr-seq '() '(proc)
			     `((movimm proc ,(car exp))))
	     (compile-instr-list (car exp) 'proc 'next)))
	  (operand-codes (map (lambda (o) (compile-instr-list o 'val 'next)) (cdr exp))))
      (preserving '(env cont)
      		  proc-code
      		  (preserving '(proc cont)
      			      (construct-arglist operand-codes)
			      (if (is-fundamental (car exp))
				  (compile-fundamental-proc-call target linkage)
				(if (is-lambda (car exp))
				    (compile-lambda-proc-call target linkage)
				  (compile-proc-call target linkage))))))))

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
	  (append-two-instr-seqs
	   (mk-instr-seq '() '(argl)
			 '((movimm argl nil)))
	   (if (is-nil (cdr operand-codes))
	       get-last-arg
	     (preserving '(env)
			 get-last-arg
			 (get-rest-args (cdr operand-codes))))))))))

(define get-rest-args
  (lambda (operand-codes)
    (let ((code-for-next-arg
	   (preserving '(argl)
		       (car operand-codes)
		       (mk-instr-seq '(val argl) '(argl)
				     '((cons argl val))))))
      (if (is-nil (cdr operand-codes))
	  code-for-next-arg
	(preserving '(env)
		    code-for-next-arg
		    (get-rest-args (cdr operand-codes)))))))


(define compile-fundamental-proc-call
  (lambda (target linkage)
      (end-with-linkage linkage
			(mk-instr-seq '(proc argl)
				      (list target)
				      '((callf))))))

(define compile-lambda-proc-call
  (lambda (target linkage)
    (let ((after-call       (mk-label "after-call"))
	  (compiled-linkage (if (= linkage 'next)
				after-call
			      linkage)))
      (append-two-instr-seqs
       (compile-proc-appl target compiled-linkage)
       after-call))))



(define compile-proc-call
  (lambda (target linkage)
    (let ((fund-branch      (mk-label "fund-branch"))
	  (compiled-branch  (mk-label "comp-branch"))
	  (after-call       (mk-label "after-call"))
	  (compiled-linkage (if (= linkage 'next)
				after-call
			      linkage)))
      (append-instr-seqs
       (list (mk-instr-seq '(proc) '()
			     `((bpf ,fund-branch)))
	     (parallel-instr-seqs
	      (append-two-instr-seqs
	       compiled-branch
	       (compile-proc-appl target compiled-linkage))
	      (append-two-instr-seqs
	       fund-branch
	       (end-with-linkage linkage
				 (mk-instr-seq '(proc argl)
					       (list target)
					       '((callf))))))
	     after-call)))))

(define compile-proc-appl
  (lambda (target linkage)
    (if (and (= target 'val) (not (= linkage 'return)))
	(mk-instr-seq '(proc) all-regs
		      `((movimm cont ,linkage)
			(cadr val proc)
			(jmp val)))
      (if (and (not (= target 'val))
	       (not (= linkage 'return)))
	  (let ((proc-return (mk-label "proc-return")))
	    (mk-instr-seq '(proc) all-regs
			  `((movimm cont ,proc-return)
			    (cadr val proc)
			    (jmp val)
			    ,proc-return
			    (mov ,target val)
			    (jmpimm ,linkage))))
	(if (and (= target 'val)
		 (= linkage 'return))
	    (mk-instr-seq '(proc continue) all-regs
			  '((cadr val proc)
			    (jmp val)))
	  'compile-error)))))
	    				 
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
	      (if (is-progn exp)
		  (compile-progn (cdr exp) target linkage)
		(if (is-let exp)
		    (compile-let exp target linkage)
		  (if (is-list exp)
		      (compile-application exp target linkage)
		    (print "Not recognized")))))))))))


(define compile-program
  (lambda (exp target linkage)
    (compile-progn exp target linkage)))

