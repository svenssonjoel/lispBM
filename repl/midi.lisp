

(patch-clear 0)
(patch-osc-set-tvp 0 0 'osc-saw 1.0 0.0) ;; patch oscillator type, vol, phase_offset
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

(midi-connect 20 0) ;; I know the keyboard is there.
