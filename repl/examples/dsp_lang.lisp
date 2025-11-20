#!/usr/bin/env -S shlbm -M 11

;; Uses up a bit of memory, launch with.
;; ../repl -M 11 -s dsp_lang.lisp

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

(defun plot-signal (infile outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile  "'")))
  (gnuplot-cmd gp (str-join `("set title '" ,title "'")))
  (gnuplot-cmd gp "set xlabel 'Sample Number'")
  (gnuplot-cmd gp "set ylabel 'Amplitude'")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,infile "' binary array=1024 format='%float' with lines title 'Waveform'")))
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

(defun plot-spectrum (infile outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))
  (gnuplot-cmd gp (str-join `("set title '" ,title "'")))
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Magnitude'")
  (gnuplot-cmd gp (str-join `("plot '" ,infile "' binary array=512 format='%float' with lines title 'Frequency Domain'")))
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

(defun plot-magnitude-phase (mag-file phase-file outfile title)
  {
  (define gp (gnuplot-open))
  (gnuplot-cmd gp "set terminal pdf size 10,8")
  (gnuplot-cmd gp (str-join `("set output '" ,outfile "'")))
  (gnuplot-cmd gp (str-join `("set multiplot layout 2,1 title '" ,title "'")))

  ;; Magnitude plot
  (gnuplot-cmd gp "set title 'Magnitude Spectrum'")
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Magnitude'")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,mag-file "' binary array=512 format='%float' with lines lw 2 title 'Magnitude'")))

  ;; Phase plot
  (gnuplot-cmd gp "set title 'Phase Spectrum'")
  (gnuplot-cmd gp "set xlabel 'Frequency Bin'")
  (gnuplot-cmd gp "set ylabel 'Phase (degrees)'")
  (gnuplot-cmd gp "set yrange [-200:200]")
  (gnuplot-cmd gp "set grid")
  (gnuplot-cmd gp (str-join `("plot '" ,phase-file "' binary array=512 format='%float' with points pt 7 ps 0.5 title 'Phase'")))

  (gnuplot-cmd gp "unset multiplot")
  (gnuplot-cmd gp "set output")
  (gnuplot-close gp)
  })

;; Example 1 plot a 440Hz sine
(define sine-sig (signal-sin 440.0))


;; Example of bracket operation
(with-file "wave.bin" "wb"
           (lambda (x) (fwrite x (sample-signal sine-sig 20000 buffer))))

(plot-signal "wave.bin" "sin440.pdf" "440Hz sine (20kHz sample rate)")

;; Example 2 plot a sum signal
(define sum-sig (signal-sum (signal-sin 440.0) (signal-sin 2400.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal sum-sig 20000 buffer))
(fclose f1)

(plot-signal "wave.bin" "sin440_plus_2400.pdf" "440Hz + 2400Hz")


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

(plot-spectrum "wave.bin" "fft_sin440_plus_2400.pdf" "Frequency Spectrum: 440Hz + 2400Hz")

;; Example 4 plot a sum signal
(define sum-sig (signal-sum (signal-sin 2000.0) (signal-cos 2000.0)))

(define f1 (fopen "wave.bin" "wb"))
(fwrite f1 (sample-signal sum-sig 20000 buffer))
(fclose f1)

(plot-signal "wave.bin" "sin2000_plus_cos2000.pdf" "sin + cos")

;; Example 5 fft

(define zero-im (bufcreate (* 1024 4)))
;; buffer already contains the sin2000 + cos2000
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

(plot-spectrum "wave.bin" "fft_sin2000_plus_cos2000.pdf" "Frequency Spectrum: 2000Hz")

;; Example 6: Magnitude and Phase plot
;; Generate sin(2000Hz) + cos(2000Hz) signal

;; 
;; sin(2*pi*f*t) + cos(2*pi*f*t) = sqrt(2) * sin(2*pi*f*t + (pi/4))
;;
;; So plotting magnitude after the FFT will show one peak at 2000Hz
;; The magnitude in point p is computed as sqrt((real[p] * real[p]) + (im[p] * im[p])).
;;
;; The information about the phase of this signal is also present
;; in the output from FFT and is obtained as atan2(im[p], real[p]).
;; 
;; In this case the phase information should show the (pi/4) shift
;; at the position corresponding to the 2000Hz peak.
;;
;; Each bin of the FFT is a filter. This filter is not "perfect"
;; in the sense that it passes exactly the desired frequency.
;; As the filters are not perfect, the 2KHz signal will show up
;; in multiple bins (neighbouring bins). This is called spectral leakage.
;;
;; This imperfection will also mean that when we try to read out the
;; (pi / 4) phase shift in the result it will not be exacly (pi / 4).
;; 



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


;; Averaging around the peak-bin  and calculating phase shift
;; Note that one does not need to divide the sums by 5 to "properly average them"
;; as it is irrelevant to the angle between the real axis and the real, imag vector.
(let ((peak-bin 102)
      (sum-real 0)
      (sum-imag 0)) {
      (loopfor j (- peak-bin 2) (<= j (+ peak-bin 2)) (+ j 1) {
            (var real (bufget-f32 fft-r2 (* j 4) 'little-endian))
            (var imag (bufget-f32 fft-im2 (* j 4) 'little-endian))
            (setq sum-real (+ sum-real real))
            (setq sum-imag (+ sum-imag imag))
            })
      (print "phase shift: " (atan2 sum-imag sum-real))
      }
  )


(define fmag (fopen "magnitude.bin" "wb"))
(fwrite fmag mag-buf)
(fclose fmag)

(define fphase (fopen "phase.bin" "wb"))
(fwrite fphase phase-buf)
(fclose fphase)

(plot-magnitude-phase "magnitude.bin" "phase.bin"
                      "magnitude_phase_spectrum.pdf"
                      "FFT Analysis: sin(2000Hz) + cos(2000Hz)")

;; TODOs:
;; - Windows and FFT (Hann, Hamming, Blackman - compare side lobe reduction)
;; - Peak detection (req. mean, std_dev)
;; - Examples illustrating dB (linear vs log magnitude plots)
;; - Spectrogram (time-frequency visualization using sliding window FFT)
;; - Frequency interpolation (parabolic/quadratic peak fitting for sub-bin accuracy)
;; - Zero-padding FFT (increase frequency resolution artificially)
;; - Inverse FFT (reconstruct time-domain signal from frequency domain)
;; - Filter design (low-pass, high-pass, band-pass using frequency domain multiplication)
;; - Cross-correlation and auto-correlation
;; - Frequency modulation (FM synthesis)
;; - Amplitude modulation (AM) and demodulation
;; - Aliasing demonstration (sample signals above Nyquist frequency)
;; - Phase unwrapping (remove discontinuities in phase plots)
;; - Compare different FFT sizes (trade-off between freq/time resolution)
