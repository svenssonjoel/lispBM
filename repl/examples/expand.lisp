
(defun macro? (x)
   (and (eq (type-of x) type-list)
        (eq (car x) 'macro)))
      

(defun macro-param-list (x)
  (ix x 1))

(defun macro-body (x)
  (ix x 2))

(defun zip (ls1 ls2)
  (match ls1
         (((? a) . (? as)) (cons (list a (car ls2)) (zip as (cdr ls2))))
         (_ nil)))

(defun macro-expand (ma)
  (let (((a . as) ma)
        (evala (eval a)))
      (if (macro? evala) {
        (var params (macro-param-list evala))
        (var env (zip params as))
        (eval env (macro-body evala))
        }
        nil)))
        
    
