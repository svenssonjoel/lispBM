;; ----------------------------------------------------------------------
;; Copyright 2020 Joel Svensson	svenssonjoel@yahoo.se

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ----------------------------------------------------------------------


;; Symbols that are used within the compiled code
(define symbol-indirections '())
(define indirection-counter 1u28)

;; Create a new indirect for a symbol:
;; If an indirection already exists for the symbol in question,
;; the existing indirection is returned.
(define new-indirection
    (lambda (sym)
      (let ((exists (lookup sym symbol-indirections)))
	(if exists
	    (cdr exists)
	    (let ((indirection (mk-sym-indirect indirection-counter))
		  (mapping (cons sym (cons (cons (sym-to-str sym) indirection) '()))))
	      (progn
		(define indirection-counter (+ 1 indirection-counter))
		(define symbol-indirections (cons mapping symbol-indirections))
		indirection))))))





(define all-regs '(env
		   proc
		   val
		   argl
		   cont))

(define all-instrs '(jmpcnt      ;; pc <- cont
		     jmpimm      ;; pc <- reg
		     jmpval      ;; pc <- val
		     movimm      ;; reg <- imm
		     lookup      ;; reg <- lookup exp
		     setglbval   ;; global-env <- cons global-env (cons symbol val)
		     push        ;; stack[sp] <- reg; sp++
		     pop         ;; reg <- stack[--sp]
		     bpf         ;; pc <- if (is-fundamental proc) (jmpaddress proc)
		     ;; pc <- if (!is-fundamental proc) (+ pc 1)
		     exenvargl   ;; env <- cons env (cons symbol (car argl)); argl <- (cdr argl)
		     exenvval    ;; env <- cons env (cons symbol val)
		     cons        ;; reg0 <- cons reg0 (cons symbol reg1)
		     consimm     ;; reg <- cons reg (cons symbol imm)
		     cdr         ;; reg0 <- cdr reg1
		     cadr        ;; reg0 <- cadr reg1
		     caddr       ;; reg0 <- caddr reg1
		     car         ;; reg0 <- car reg1
 		     callf       ;; val <- fund-apply proc argl
                     done))      ;; Computation done

;; OpCode to size in bytes (including arguments)
(define instr-size
    '((jmpcnt       1)
      (jmpimm       5) ;; 5 bytes is overkill
      (jmpval       1)
      (movimm       6)
      (mov          3)
      (lookup       6)
      (setglbval    5)
      (push         2)
      (pop          2)
      (bpf          5) ;; 5 bytes is overkill
      (exenvargl    5)
      (exenvval     5)
      (cons         3)
      (consimm      6)
      (cdr          3)
      (cadr         3)
      (caddr        3)
      (callf        1)
      (done         1)
      (label        0)))


(define is-symbol
    (lambda (exp) (= type-symbol (type-of exp))))

(define is-label
    (lambda (exp) (= (car exp) 'label)))

(define get-label
    (lambda (exp) (car exp)))


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


(define label-loc '())

(define locate-labels
    (lambda (s)
      (let ((f
	     (lambda (x acc)
	       (if (is-nil x) acc
		   (progn
		     (if (is-label (car x))
			 (define label-loc (cons (cons (car x) acc) label-loc))
			 nil)
		     (f (cdr x) (+ (lookup (car (car x)) instr-size) acc)))))))
	(f (car (cdr (cdr s))) 0))))




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

;; Compiling a quoted expression results in
;; code that re-creates the quoted expression on the heap.
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
      (let ((i (new-indirection exp)))
	(end-with-linkage linkage
			  ;; Implied that the lookup looks in env register
			  (mk-instr-seq '(env)
					(list target)
					`((lookup ,target ,i)))))))
(define compile-def
    (lambda (exp target linkage)
      (let ((var (car (cdr exp)))
	    (i (new-indirection var))
	    (get-value-code
	     (compile-instr-list (car (cdr (cdr exp))) 'val 'next)))
	(end-with-linkage linkage
			  (append-two-instr-seqs get-value-code
						 (mk-instr-seq '(val) (list target)
							       `((setglbval ,i)
								 (movimm ,target ,i))))))))

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
	    (var (car keyval))
	    (i (new-indirection var)))
	(append-two-instr-seqs get-value-code
			       (mk-instr-seq '(val) (list 'env)
					     `((exenvval ,i)))))))


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
				`((exenvargl ,p))))
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

(define add-fst-argument
    (lambda (arg-code)
      (append-two-instr-seqs
       arg-code
       (mk-instr-seq '(val) '(argl)
		     '((movimm argl ())
		       (cons argl val))))))


(define construct-arglist
    (lambda (codes)
      (let ((operand-codes (reverse codes)))
	(if (is-nil operand-codes)
	    (mk-instr-seq '() '(argl)
			  '((movimm argl ())))
	    (if (is-nil (cdr operand-codes))
		(add-fst-argument (car operand-codes))
		(preserving '(env)
			    (add-fst-argument (car operand-codes))
			    (get-rest-args (cdr operand-codes))))))))

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
			  (jmpval)))
	  (if (and (not (= target 'val))
		   (not (= linkage 'return)))
	      (let ((proc-return (mk-label "proc-return")))
		(mk-instr-seq '(proc) all-regs
			      `((movimm cont ,proc-return)
				(cadr val proc)
				(jmpval)
				,proc-return
				(mov ,target val)
				(jmpimm ,linkage))))
	      (if (and (= target 'val)
		       (= linkage 'return))
		  (mk-instr-seq '(proc continue) all-regs
				'((cadr val proc)
				  (jmpval)))
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
    (lambda (exp)
      (append-two-instr-seqs
       (compile-progn exp 'val 'next)
       (mk-instr-seq () ()
		     '((done))))))

(define ops-out
    (lambda (lab ops)
      (if ops 
	  (if (is-label (car ops))
	      (ops-out (car ops) (cdr ops))
	      (progn (asm-out lab (car ops))
		     (ops-out nil (cdr ops))))
	  (print "done"))))

(define gen-asm
    (lambda (prg)
      (let ((ir (compile-program prg))
	    (ir-ops (car (cdr (cdr ir)))))
	(progn
	  (ops-out nil ir-ops)
	  symbol-indirections
	  ))))
