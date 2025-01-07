
;; defstruct attempt

(defun zip (ls1 ls2)
  (match ls1
         (((? a) . (? as)) (cons (list a (car ls2)) (zip as (cdr ls2))))
         (_ nil)))

(defun create-struct (name num-fields) {
  (var arr (mkarray (+ 1 num-fields)))
  (setix arr 0 name)    
  arr
  })

(defun accessor-sym (name field)
  (str2sym (str-merge name "-" (sym2str field))))

(defun access-set (i)
  (lambda ( struct)
    (if (rest-args)
        (setix struct i (rest-args 0))
      (ix struct i))))
  

(define defstruct
  (macro (name list-of-fields)
         {
         (print "a")
         (var num-fields (length list-of-fields))
         (var name-as-string (sym2str name))
         (var new-create-sym (str2sym (str-merge "create-" name-as-string)))
         (var field-ix (zip list-of-fields (range 1 (+ num-fields 1))))
         `(progn
            (define ,new-create-sym (lambda () (create-struct ',name ,num-fields)))
            ,@(map (lambda (x) (list define (accessor-sym name-as-string (car x))
                                     (access-set (car (cdr x))))) field-ix)
            't
            )
         }))


