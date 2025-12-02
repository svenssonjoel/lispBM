#!/usr/bin/env -S shlbm -M 512000 --

(import "dsp_lang.lisp")

;; in radio mixing is multiplication.
;; This example shows DSB-SC (Double-Sideband Suppressed Carrier)  modulation

(define baseband-sig (signal-sin 1000.0))   ;; 1000Hz baseband signal
(define carrier-sig  (signal-sin 100000.0)) ;; 100KHz carrier

(define transmit-sig (signal-prod baseband-sig carrier-sig))

(define tx-buffer (bufcreate (* 4 1024)))

(sample-signal transmit-sig 500000.0 tx-buffer)

(define rx-buffer (bufcreate (* 4 1024)))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var time (/ i 500000.0))
      (var s    (bufget-f32 tx-buffer (* i 4) 'little-endian))
      (var c    (sin (* 100000.0 two-pi time)))
      (var m    (* s c))
      (bufset-f32 rx-buffer (* i 4) m 'little-endian)
      })


(defun lowpass (input-buffer output-buffer alpha) {
      (var num-samples (/ (length input-buffer) 4))
      (var prev 0.0)

      (loopfor i 0 (< i num-samples) (+ i 1) {
          (var input (bufget-f32 input-buffer (* i 4) 'little-endian))
          (setq prev (+ (* alpha input) (* (- 1.0 alpha) prev)))
          (bufset-f32 output-buffer (* i 4) prev 'little-endian)
      })
  })

(define filtered-buffer (bufcreate (* 4 1024)))

;; Demodulated signal
(lowpass rx-buffer filtered-buffer 0.1)

(with-file "wave1.bin" "wb"
           (lambda (x) (fwrite x tx-buffer)))

(with-file "wave2.bin" "wb"
           (lambda (x) (fwrite x filtered-buffer)))

(print "output: mixing1.pdf")
(plot-signal-signal "wave1.bin" "wave2.bin" "mixing1.pdf"
                    "Demodulation of DSB-SC modulated signal"
                    "Carrier * baseband"
                    "Demodulated signal")

;; Experiment: Is low-pass filtering enough to recover the base-band signal?
;; The result shows that low-pass filtering is not enough.
(lowpass tx-buffer filtered-buffer 0.01) ;; filter very strongly!

(with-file "wave1.bin" "wb"
           (lambda (x) (fwrite x tx-buffer)))

(with-file "wave2.bin" "wb"
           (lambda (x) (fwrite x filtered-buffer)))

(print "output: mixing2.pdf")
(plot-signal-signal "wave1.bin" "wave2.bin" "mixing2.pdf"
                    "Experiment with filtering"
                    "Carrier * baseband"
                    "Low-pass filtering mixed signal")


