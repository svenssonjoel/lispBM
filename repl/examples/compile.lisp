
;; Lbm compiler experiment

;; Compile function bodies.
;; Do not recursively compile into functions called from body.
;; Try to utilize as far as possible all existing code (continuaions, apply, fundamentals..).
;; Emacs elisp like behavior as I understand it.

;; Compiling a lambda leads to a kind of compiled closure that can be applied.
;; The point of compilation determines the values of free variables inside
;; the lambda body.

;; Should it be possible to compile for example a "progn"
;; as a standalone thing? 

;; Fundamental operations on literals can be evaluated at compile time.

;; Bytecode format
;; Lisp array containing:
;;  1. number of arguments 
;;  2. byte-array with byte-code
;;  3. lisp array of "constants"

;; IR format
;; List containing:
;; 1. list of arguments - for index backwards lookup ?
;; 2. list of opcode/arguments
;; 3. list of constants

;; Not sure we need arguments. argument references could be done with an index.
;; - What about things like rest-args..
;;

;; closure application gets an environment populted with param/value pairs for
;; the arguments. For compiled code, it may be easier to push the param values
;; to the stack and then generate code with stack-index operations where argument
;; accesses take place. 

(defun mkir ()
  (let ((ir (mkarray 2)))
    (setix ir 0 nil)
    (setix ir 1 nil)))

(defun is-symbol (e)
  (eq (type-of e) type-symbol))

(defun is-list (e)
  (eq (type-of e) type-list))

(defun is-symbol (e)
  (eq (type-of e) type-symbol))

(defun is-number (e)
  (let ((type (type-of e)))
    (or (eq type type-char) ;; symbol 91  
        (eq type type-byte) ;; symbol 92  This is not that good.
        (eq type type-i)
        (eq type type-u))))

(defun is-zero (e)
  (if (is-number e) 
      (= 0 e)
    nil))

(defun is-not-zero (e)
  (not (is-zero e)))

(def fundamentals
     (list '+
           '-
           '*
           '/
           'mod
           '=
           )) ;; to start with

(def special-forms
     (list 'define
           'let));; to start with

(defun is-fundamental (f)
  (member fundamentals f))

(defun is-special-form (f)
  (member special-forms f))

(defun is-closure (f)
  (eq (ix f 0) 'closure))

(defun filter (f xs)
  (match xs
         ( ((? x) . (? xs)) (if (f x) (cons x (filter f xs))
                              (filter f xs)))
         ( nil nil)))

(defun index-lookup (x xs)
  (let ( (find-ix (lambda (x xs inx)
                    (match xs
                           ( ((? y) . (? ys)) (eq y x) inx )
                           ( ((? y) . (? ys)) (find-ix x ys (+ inx 1)))
                           ( _ 'nil)))))
    (find-ix x xs 0)))

(defun compile-list (ps const xs)
  (match xs
         ( ((? x) . (? xs))
           {
           (print "Compiling " x )
           (var (ps-new const-new code) (compile ps const x))
           (var (ps-fin const-fin code-fin) (compile-list ps-new const-new xs))
           (print "code " code)
           (print "code-fin " code-fin)
           (print "code-res " (append code code-fin))
           (list ps-fin const-fin (append code code-fin))
           }
           )
         ( _ (list ps const nil))))


;; Code specific to associative operator with 0 identity.
;; TODO: generalize
(defun compile-fundamental (ps const x xs)
  {
  (var constants (filter (lambda (x) (is-number x)) xs))
  (var unknown   (filter (lambda (x) (not (is-number x))) xs))
  (var new-e (eval (cons x constants)))
  (var new-xs (cons new-e unknown))
  (var xs-1 (filter is-not-zero new-xs))
  (print "new-e " new-e)
  (print "new-xs " new-xs)
  (print "xs-1 " xs-1)
  
  (var (ps-new const-new code) (compile-list ps const xs-1))
  (print "Generatin code " (append code (list (list x (length xs-1)))))
  (list ps-new const-new (append code (list (list x (length xs-1)))))
  })

(defun compile-special-form (ps const x xs)
  (print "special form"))

(defun compile (ps const body)
  (match body
         ( ((? x) . (? xs)) (cond ((is-fundamental x) (compile-fundamental ps const x xs))
                                  ((is-special-form x) (compile-special-form ps const x xs))))
         ( (? x) (cond ((is-symbol x) (let (( inx (index-lookup x ps)))
                                        (if inx
                                            (list ps const `((stack-ref ,inx)))
                                          (print "Cannot compile " x))))
                       ((is-number x) (let (( inx (index-lookup x const))
                                            ( n   (length const)))
                                        (if inx
                                            (list ps const `((constant ,(- length inx))))
                                          (list ps (cons x const) `((constant ,n)))))) ;; Maybe + n 1
                       (t             (list ps const nil))
                       ))))                       
                        
           

(defun compile-fun (e)
  (match e
         ( (lambda (? ps) (? body))
           {
           (var (ps-fin const-fin code-fin) (compile ps nil body))
           (list ps-fin const-fin code-fin)
           }
           )
         ( _ (print "Can only compile lambdas"))
         ))

(compile-fun '(apa))

;; (define apa 10)

;; (exec-code (compile '(apa)))

(compile-fun '((f 1 2 3)))

(compile-fun '(lambda (x) (+ x 1 )))

