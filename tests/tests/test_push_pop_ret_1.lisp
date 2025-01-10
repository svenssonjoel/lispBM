

(defun f (x)
  (if (< x 10)
      (pop-ret 100)
    x))


(check (and (eq (push-ret (pop-ret)) nil)
            (eq (push-ret {(push-ret (pop-ret 100)) (pop-ret 99)}) 99)
            (eq (push-ret (+ 1 (f 5))) 100)))
