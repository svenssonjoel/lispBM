; Test inverse FFT and round-trip FFT -> IFFT

(defun approx-equal (x y tolerance)
  (< (abs (- x y)) tolerance))

(defun test-fft-inverse-basic ()
  ; Test that inverse FFT flag is accepted
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    (bufset-f32 real 0 1.0f32)
    (bufset-f32 real 4 2.0f32)
    (bufset-f32 real 8 3.0f32)
    (bufset-f32 real 12 4.0f32)
    (bufclear imag 0 0 16)
    (let ((result (fft real imag 'inverse)))
      (not (eq result nil)))))

(defun test-fft-roundtrip ()
  ; Test FFT followed by inverse FFT recovers original signal
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    ; Create a simple signal
    (bufset-f32 real 0 1.0f32)
    (bufset-f32 real 4 2.0f32)
    (bufset-f32 real 8 3.0f32)
    (bufset-f32 real 12 4.0f32)
    (bufclear imag 0 0 16)

    ; Forward FFT
    (let ((fft-result (fft real imag)))
      (if (eq fft-result nil)
        nil
        (let ((real-fft (car fft-result))
              (imag-fft (cdr fft-result)))
          ; Inverse FFT
          (let ((ifft-result (fft real-fft imag-fft 'inverse)))
            (if (eq ifft-result nil)
              nil
              (let ((real-ifft (car ifft-result))
                    (imag-ifft (cdr ifft-result)))
                ; Check if we recovered the original signal (within tolerance)
                (and (approx-equal (bufget-f32 real-ifft 0) 1.0f32 0.001f32)
                     (approx-equal (bufget-f32 real-ifft 4) 2.0f32 0.001f32)
                     (approx-equal (bufget-f32 real-ifft 8) 3.0f32 0.001f32)
                     (approx-equal (bufget-f32 real-ifft 12) 4.0f32 0.001f32)
                     (approx-equal (bufget-f32 imag-ifft 0) 0.0f32 0.001f32)
                     (approx-equal (bufget-f32 imag-ifft 4) 0.0f32 0.001f32))))))))))

(defun test-fft-inverse-dc ()
  ; Inverse FFT of DC frequency should give constant signal
  (let ((real (bufcreate 16))
        (imag (bufcreate 16)))
    ; Set only DC component
    (bufset-f32 real 0 4.0f32)
    (bufset-f32 real 4 0.0f32)
    (bufset-f32 real 8 0.0f32)
    (bufset-f32 real 12 0.0f32)
    (bufclear imag 0 0 16)

    (let ((result (fft real imag 'inverse)))
      (if (eq result nil)
        nil
        (let ((real-out (car result))
              (imag-out (cdr result)))
          ; Should get constant 1.0 in all positions (4.0 / 4 samples)
          (and (approx-equal (bufget-f32 real-out 0) 1.0f32 0.001f32)
               (approx-equal (bufget-f32 real-out 4) 1.0f32 0.001f32)
               (approx-equal (bufget-f32 real-out 8) 1.0f32 0.001f32)
               (approx-equal (bufget-f32 real-out 12) 1.0f32 0.001f32)))))))

(defun run-tests ()
  (and (test-fft-inverse-basic)
       (test-fft-roundtrip)
       (test-fft-inverse-dc)))

(if (run-tests)
  (print "SUCCESS")
  (print "FAIL"))
