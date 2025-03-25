
(defun additions ()
  (and (> (+ (* (+ 1.0f32 2) 2) 1) 1)
       (> (+ (* (+ 2.0f32 2) 2) 1) 1)
       (> (+ (* (+ 3.0f32 2) 2) 1) 1)
       (> (+ (* (+ 4.0f32 2) 2) 1) 1)))



(defun test-it (n acc)
  (if (= n 0) acc
    (test-it (- n 1) (and acc (additions) (additions) (additions)))
    ))

(defun mem-waster () {
       (var a (range 5))
       (mem-waster)
       })

;;(define thd-id (spawn mem-waster))

(check (test-it 100000 t))

;;(kill thd-id t)
