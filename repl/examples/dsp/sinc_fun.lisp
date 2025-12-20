#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")


(define samplerate 20000)
(define window-time (/ 1024.0 samplerate))

(defun sinc (x)
  (if (= x 0)
      1.0
      (/ (sin x) x)))


(define scale-to-window (/ 1.0 window-time))

;; center sinc function in the sampling window
(let ((buffer (bufcreate (* 4 1024))))
  {
  (sample-signal (signal-fun (lambda (x) (sinc (- (* 2 12 scale-to-window x) 12))))  samplerate buffer)
  (with-file "wave.bin" "wb"
             (lambda (x) (fwrite x buffer)))
  (plot-signal "wave.bin" "sinc1.pdf"
               "sinc function")
  })
