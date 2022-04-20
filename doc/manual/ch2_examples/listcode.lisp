
(defun elem (ls e)
  (if (eq ls 'nil)
      'nil
    (if (eq (first ls) e)
        e
      (elem (rest ls) e))))

(defun elem-pm (ls e)
  (match ls
         ( nil nil )
         ( ((? x) . (? xs))
           (if (eq x e) e (elem-pm xs e)) )))

(defun elem-pm2 (ls e)
  (match ls
         ( nil nil )
         ( (,e . _) e )
         ( (_ . (? xs)) (elem-pm2 xs e) )))

(defun length-notail (ls)
  (if (eq ls nil)
      0
    ( + 1 (length-notail (rest ls)))))


(defun length-tail (ls)
  (let ((len-helper (lambda (acc ls)
                      (if (eq ls nil)
                          acc
                        (len-helper (+ 1 acc) (rest ls))))))
    (len-helper 0 ls)))
