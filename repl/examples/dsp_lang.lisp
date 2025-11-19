

;; EDSL for dsp programming/learning/experimenting

(define pi 3.14159)
(define two-pi (* 2 pi))

;; Constructors for Deeply embedded time dependent signals
(defun signal-sin (f)
  (if (rest-args)
      (list 'signal-sin f (rest-args 0))
      (list 'signal-sin f 0.0)))
(defun signal-cos (f)
  (if (rest-args)
      (list 'signal-cos f (rest-args 0))
      (list 'signal-cos f 0.0)))

(defun signal-const (v)
  (list 'signal-const v))
    

;; Signal operators
(defun signal-sum (s1 s2)
  (list 'signal-sum s1 s2))
(defun signal-prod (s1 s2)
  (list 'signal-prod s1 s2))

(define signal-phase-shift 'signal-phase-shift)


;; evaluate a signal at time t
(defun eval-signal (s sig-t)
  (match s
   ( (signal-sin (? f) (? p)) (sin (+ (* f two-pi sig-t) p)))
   ( (signal-cos (? f) (? p)) (cos (+ (* f two-pi sig-t) p)))
   ( (signal-const (? v)) v)
   ((signal-sum (? s1) (? s2)) (+ (eval-signal s1 sig-t) (eval-signal s2 sig-t)))
   ((signal-prod (? s1) (? s2)) (* (eval-signal s1 sig-t) (eval-signal s2 sig-t)))
   ((signal-phase-shift (? s) (? p) (eval-signal s (+ sig-t p))))
   ))


(defun sample-signal (s sample-rate buffer ) {
       (var num-samples (/ (length buffer) 4)) 
       (var time-delta (/ 1.0 sample-rate))
       (var sig-t 0.0)
       (loopfor i 0 (< i num-samples) (+ i 1) {
             (bufset-f32 buffer (* i 4) (eval-signal s sig-t) 'little-endian)
             (setq sig-t (+ sig-t time-delta))
             })
       buffer
       })


;; Filehandling "bracket style" operation

(defun with-file (filename mode operation) {
       (var fh (fopen filename mode))
       (operation fh)
       (fclose fh)
       })




(define buffer (bufcreate (* 4 1024)))



;; Example 1 plot a 440Hz sine
(define sine-sig (signal-sin 440.0))


;; Example of bracket operation
(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x (sample-signal sine-sig 20000 buffer))))
(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf")
(gnuplot-cmd gp "set output 'sin440.pdf'")
(gnuplot-cmd gp "set title 'DSP Signal (20kHz sample rate)'")
(gnuplot-cmd gp "set xlabel 'Sample Number'")
(gnuplot-cmd gp "set ylabel 'Amplitude'")
(gnuplot-cmd gp "set grid")
(gnuplot-cmd gp "plot 'wave.bin' binary array=1024 format='%float' with lines title 'Waveform'")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)

;; Example 2 plot a sum signal
(define sum-sig (signal-sum (signal-sin 440.0) (signal-sin 2400.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal sum-sig 20000 buffer))
(fclose f1)

(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf")
(gnuplot-cmd gp "set output 'sin440_plus_2400.pdf'")
(gnuplot-cmd gp "set title 'DSP Signal (20kHz sample rate)'")
(gnuplot-cmd gp "set xlabel 'Sample Number'")
(gnuplot-cmd gp "set ylabel 'Amplitude'")
(gnuplot-cmd gp "set grid")
(gnuplot-cmd gp "plot 'wave.bin' binary array=1024 format='%float' with lines title 'Waveform'")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)
;; Example 3 fft

(define zero-im (bufcreate (* 1024 4)))
;; buffer already contains the 440 + 2400 signal
(define fft-res (fft buffer zero-im 'little-endian))
(define fft-r (car fft-res))
(define fft-im (cdr fft-res))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var x (bufget-f32 fft-r (* i 4) 'little-endian))
      (var y (bufget-f32 fft-im (* i 4) 'little-endian))
      (bufset-f32 fft-r (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
      })


(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 fft-r)
(fclose f1)

(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf")
(gnuplot-cmd gp "set output 'fft_sin440_plus_2400.pdf'")
(gnuplot-cmd gp "set title 'Frequency Spectrum: 440Hz + 2400Hz'")
(gnuplot-cmd gp "set xlabel 'Frequency Bin'")
(gnuplot-cmd gp "set ylabel 'Magnitude'")
(gnuplot-cmd gp "plot 'wave.bin' binary array=512 format='%float' with lines title 'Frequency Domain'")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)
;; Example 4 plot a sum signal
(define sum-sig (signal-sum (signal-sin 440.0) (signal-cos 440.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal sum-sig 20000 buffer))
(fclose f1)

(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf")
(gnuplot-cmd gp "set output 'sin2000_plus_cos2000.pdf'")
(gnuplot-cmd gp "set title 'DSP Signal (20kHz sample rate)'")
(gnuplot-cmd gp "set xlabel 'Sample Number'")
(gnuplot-cmd gp "set ylabel 'Amplitude'")
(gnuplot-cmd gp "set grid")
(gnuplot-cmd gp "plot 'wave.bin' binary array=1024 format='%float' with lines title 'Waveform'")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)
;; Example 4 fft

(define zero-im (bufcreate (* 1024 4)))
;; buffer already contains the 440 + 2400 signal
(define fft-res (fft buffer zero-im 'little-endian))
(define fft-r (car fft-res))
(define fft-im (cdr fft-res))

(loopfor i 0 (< i 1024) (+ i 1) {
      (var x (bufget-f32 fft-r (* i 4) 'little-endian))
      (var y (bufget-f32 fft-im (* i 4) 'little-endian))
      (bufset-f32 fft-r (* i 4) (sqrt (+ (* x x) (* y y))) 'little-endian)
      })


(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 fft-r)
(fclose f1)

(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf")
(gnuplot-cmd gp "set output 'fft_sin2000_plus_cos2000.pdf'")
(gnuplot-cmd gp "set title 'Frequency Spectrum: 2000Hz'")
(gnuplot-cmd gp "set xlabel 'Frequency Bin'")
(gnuplot-cmd gp "set ylabel 'Magnitude'")
(gnuplot-cmd gp "plot 'wave.bin' binary array=512 format='%float' with lines title 'Frequency Domain'")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)

;; Example 5: Magnitude and Phase plot
;; Generate sin(2000Hz) + cos(2000Hz) signal
(define phase-test-sig (signal-sum (signal-sin 2000.0) (signal-cos 2000.0)))
(sample-signal phase-test-sig 20000 buffer)

(define zero-im2 (bufcreate (* 1024 4)))
(define fft-res2 (fft buffer zero-im2 'little-endian))
(define fft-r2 (car fft-res2))
(define fft-im2 (cdr fft-res2))

(define mag-buf (bufcreate (* 512 4)))
(define phase-buf (bufcreate (* 512 4)))

;; Compute magnitude and phase for first 512 bins (up to Nyquist)
;; Convert radians to degrees: degrees = radians * 180 / pi
(define rad-to-deg (/ 180.0 3.14159))
(loopfor i 0 (< i 512) (+ i 1) {
      (var real (bufget-f32 fft-r2 (* i 4) 'little-endian))
      (var imag (bufget-f32 fft-im2 (* i 4) 'little-endian))
      (var mag (sqrt (+ (* real real) (* imag imag))))
      (var phase-rad (atan2 imag real))
      (var phase-deg (* phase-rad rad-to-deg))
      (bufset-f32 mag-buf (* i 4) mag 'little-endian)
      (bufset-f32 phase-buf (* i 4) phase-deg 'little-endian)
      })

(define fmag (fopen "magnitude.bin" "wb"))
(fwrite fmag mag-buf)
(fclose fmag)

(define fphase (fopen "phase.bin" "wb"))
(fwrite fphase phase-buf)
(fclose fphase)

(define gp (gnuplot-open))
(gnuplot-cmd gp "set terminal pdf size 10,8")
(gnuplot-cmd gp "set output 'magnitude_phase_spectrum.pdf'")
(gnuplot-cmd gp "set multiplot layout 2,1 title 'FFT Analysis: sin(2000Hz) + cos(2000Hz)'")

;; Magnitude
(gnuplot-cmd gp "set title 'Magnitude Spectrum'")
(gnuplot-cmd gp "set xlabel 'Frequency Bin'")
(gnuplot-cmd gp "set ylabel 'Magnitude'")
(gnuplot-cmd gp "set grid")
(gnuplot-cmd gp "plot 'magnitude.bin' binary array=512 format='%float' with lines lw 2 title 'Magnitude'")

;; Phase
(gnuplot-cmd gp "set title 'Phase Spectrum'")
(gnuplot-cmd gp "set xlabel 'Frequency Bin'")
(gnuplot-cmd gp "set ylabel 'Phase (degrees)'")
(gnuplot-cmd gp "set yrange [-200:200]")
(gnuplot-cmd gp "set grid")
(gnuplot-cmd gp "plot 'phase.bin' binary array=512 format='%float' with points pt 7 ps 0.5 title 'Phase'")

(gnuplot-cmd gp "unset multiplot")
(gnuplot-cmd gp "set output")
(gnuplot-close gp)




