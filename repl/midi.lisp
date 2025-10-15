



(defun violin (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.4 0.1 0.7 0.15)
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.6 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.4 0.0)
  (patch-mod-set patch-num 1 0 'mod-env 8.0)
  (patch-lfo-set patch-num 0 'osc-sine 5.5)
  (patch-mod-set patch-num 0 1 'mod-lfo1 3.0)
  (patch-mod-set patch-num 1 1 'mod-lfo1 3.5)
  (patch-filter-set patch-num 'simple-lpf 2800.0)
  })

(patch-clear 0)
(violin 0)

;; Play a simple melody
(note-on 0 64 90)  ;; E4
(sleep 0.5)
(note-off 0 64)

(note-on 0 67 95)  ;; G4
(sleep 0.5)
(note-off 0 67)

(note-on 0 71 100) ;; B4
(sleep 0.8)
(note-off 0 71)


;; oscillator 0 saw
;;(patch-osc-tvp-set 0 0 'osc-saw 0.5 0.0)
;; oscillator 1 sine
;;(patch-osc-tvp-set 0 1 'osc-sine 0.5 0.0) 
;;(patch-adsr-set 0 0.01 0.1 0.7 0.2)

;;(print (patch-adsr-get 0))


;;(patch-filter-set 0 'simple-lpf 8000)
;;(patch-filter-set 0 'simple-hpf 200)

;; patch 0 lfo 0 20.0hz sine wave
;;(patch-lfo-set 0 0 'osc-sine 5.0) 

;; Patch 0 oscillator 0 modulator 0 lfo1 0.1
;;(patch-mod-set 0 0 0 'mod-lfo1 5.0) 

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
