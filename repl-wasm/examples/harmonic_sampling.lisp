(import "dsp_lang.lisp" 'dsp)
(read-eval-program dsp)


;; This example illustrates harmonic-sampling.

(define sample-rate 60000)
(define carrier-hz  100000)
(define baseband-hz 100)
(define n-samples (* 1.0 sample-rate))
(define magnitudes (bufcreate (* 4 65536)))

(define data-r     (bufcreate (* 4 n-samples)))
(define data-im    (bufcreate (* 4 n-samples)))
(bufclear data-im 0 0 (* 4 n-samples))

(define carrier (signal-sin carrier-hz))
(define baseband (signal-sin baseband-hz))

(define sig (signal-prod carrier baseband))


;; Sample signal
(sample-signal-from sig sample-rate 0.0 data-r)

(wasm-plot data-r "Samples");

;; FFT and magnitude spectrum
(let ((fft-result (fft data-r data-im 'little-endian))
      (n (/ (buflen (car fft-result)) 4))) {
  (loop ((i 0)) (< i n) {
    (var x (bufget-f32 (car fft-result) (* i 4) 'little-endian))
    (var y (bufget-f32 (cdr fft-result) (* i 4) 'little-endian))
    (bufset-f32 magnitudes (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
    (setq i (+ i 1))
  })

  (wasm-plot magnitudes "FFT Magnitude Spectrum")
})


