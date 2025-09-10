
(defun assp (p al)
  (if (eq al nil) nil
    (if (p (car (car al)))
        (car al)                ; return the match
        (assp p (cdr al)))))    ; continue searching

(defun pair? (a)
  (match a ((_ . _) t)
           (_ nil)))
(defun kan-var (c) (list-to-array (list c)))
(defun kan-var? (x) (array? x))
(defun kan-var=? (x1 x2) (= (ix x1 0) (ix x2 0)))

(defun walk (u s)
  (let ((pr (and (kan-var? u) (assp (lambda (v) (kan-var=? u v)) (ix s 0)))))
    (if pr (walk (cdr pr) s) u)))

(defun extend-s (x v s) 
  (let ((new-list `((,x . ,v) . ,(ix s 0))))
    (list-to-array (list new-list))))

(defun kan== (u v)
  (lambda (sc)
    (let ((s (unify u v (car sc))))
      (if s (unit `(,s . ,(cdr sc))) mzero))))

(defun unit (s/c) (cons s/c mzero))
(define mzero '())

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (kan-var? u) (kan-var? v) (kan-var=? u v)) s)
      ((kan-var? u) (extend-s u v s))
      ((kan-var? v) (extend-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (t (and (eq u v) s)))))

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (kan-var c)) `(,(car s/c) . ,(+ c 1))))))

(defun disj (g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(defun conj (g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(defun mplus (a b)
  (match a
         (nil b)
         ((closure _ _ _ _) (lambda () (mplus b (a))))
         (_ (cons (car a) (mplus (cdr a) b)))))

(defun bind (a g)
  (match a
         (nil mzero)
         ((closure _ _ _ _) (lambda () (bind (a) g)))
         (_ (mplus (g (car a)) (bind (cdr a) g)))))


(define empty-sub [| () |]) ;; truthy array holding empty list
(define empty-state `(,empty-sub . 0))

;; (let ((a ((call/fresh (lambda (q) (kan== q 5))) empty-state)))
;;   (car a))

;;'(((#(0) . 5)) . 1))

(define a-and-b
  (conj 
   (call/fresh (lambda (a) (kan== a 7)))
   (call/fresh 
    (lambda (b) 
      (disj
       (kan== b 5)
       (kan== b 6))))))

(define love
  (call/fresh (lambda (res)
                (call/fresh (lambda (a)
                              (call/fresh (lambda (b)
                                            (call/fresh (lambda (c)
                                                          (conj (kan== a 'i)
                                                                (conj 
                                                                 (kan== b 'love)
                                                                 (conj 
                                                                  (kan== c 'you)
                                                                  (kan== res (list a b c))))))))))))))

(define var-subst (lambda (ls as)
                    (if (eq nil ls ) nil
                      (let (( a (car ls))
                            ( b (assoc as a)))
                        (if b (cons b (var-subst (cdr ls) as))
                          (cons a (var-subst (cdr ls) as)))))))
                    
(cdr (let ((res (love empty-state))) 
       (let ((subst (ix (car (car res)) 0)))
         (var-subst subst subst))))


;; (test-check "second-set t1"
;;   (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
;;     (car $))
;;   '(((#(0) . 5)) . 1))


;; Define a relation for natural numbers using Peano arithmetic
;; nat(zero) and nat(succ(X)) if nat(X)
(define nato
    (lambda (x)
      (disj
       (kan== x 'zero)
       (call/fresh (lambda (n)
                     (conj
                      (kan== x `(succ ,n))
                      (nato n)))))))

;; Define addition relation: plus(X, Y, Z) means X + Y = Z
;;plus(zero, Y, Y)
;;plus(succ(X), Y, succ(Z)) if plus(X, Y, Z)
(define pluso
    (lambda (x y z)
      (disj
       (conj (kan== x 'zero) (kan== y z))
       (call/fresh (lambda (x1)
                     (call/fresh (lambda (z1)
                                   (conj
                                    (kan== x `(succ ,x1))
                                    (conj
                                     (kan== z `(succ ,z1))
                                     (pluso x1 y z1))))))))))


(define two '(succ (succ zero)))
(define three '(succ (succ (succ zero))))
(define five '(succ (succ (succ (succ (succ zero))))))


(define verify-addition
    (pluso two three five))

(print "Proving 2 + 3 = 5:")
(let ((result (verify-addition empty-state)))
  (if (eq result mzero)
      (print "FAILED")
      (print "SUCCESS: 2 + 3 = 5 is proven!")))

;; Test a false statement: 2 + 3 = 7
(define seven '(succ (succ (succ (succ (succ (succ (succ zero))))))))
(define false-addition (pluso two three seven))

(print "Testing false statement: 2 + 3 = 7:")
(let ((result (false-addition empty-state)))
  (if (eq result mzero)
      (print "CORRECTLY FAILED: 2 + 3 != 7")
      (print "ERROR: False statement succeeded!")))

;; Test simple cases first
(print "Testing simple unification (5 = 5):")
(let ((result ((kan== 5 5) empty-state)))
  (if (eq result mzero)
      (print "FAILED")
      (print "SUCCESS: Simple unification works")))

(print "Testing 0 + 0 = 0:")
(let ((result ((pluso 'zero 'zero 'zero) empty-state)))
  (if (eq result mzero)
      (print "FAILED")
      (print "SUCCESS: 0 + 0 = 0 proven!")))
