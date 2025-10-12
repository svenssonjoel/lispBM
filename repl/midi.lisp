

(patch-clear 0)
;; oscillator 0 saw
(patch-osc-tvp-set 0 0 'osc-saw 0.5 0.0)
;; oscillator 1 sine
(patch-osc-tvp-set 0 1 'osc-sine 0.5 0.0) 
(patch-adsr-set 0 0.01 0.1 0.7 0.2)

(defun midi-thd ()
  (loopwhile t {
        (match (midi-read)
               (nil nil)
               ((note-on (? n) (? v)) {
                (print "note on: " n " " v )
                (note-on 0 n v)
                })
               ((note-off (? n) _ )      (note-off 0 n))
               )
        }))

(spawn midi-thd)

(define music
    '((76 100)
      (71 100)
      (72 100)
      (74 100)
      (72 100)
      (71 100)
      (70 100)))


(defun play (x)
  (if (eq x nil) ()
      (let (((note vel) (car x))) {
           (note-on 0 note vel)
           (sleep 0.2)
           (note-off 0 note)
           (play (cdr x))
           })))


;(midi-connect 20 0) ;; I know the keyboard is there.
