(define formats '(indexed2 indexed4 indexed16 rgb332 rgb565 rgb888))

;; List of (width height iterations). Just 640x480 for now 
(define canvases (list (list 640 480 500)))

(define color-for-format
  (list (list 'indexed2 1)
        (list 'indexed4 3)
        (list 'indexed16 15)
        (list 'rgb332 0x3388CC)
        (list 'rgb565 0x3388CC)
        (list 'rgb888 0x3388CC)))

(defun color-of (fmt) (car (assoc color-for-format fmt)))

(define csv-fd (f-open csv-filename "w"))
(f-write-str csv-fd "shape,format,canvas_w,canvas_h,param,iterations,total_s,us_per_call\n")

(defun csv-row (fields)
  (f-write-str csv-fd (str-merge (str-join fields ",") "\n")))

;; ---- ppm debug dump ----
;; img-getpix returns the raw palette index (not a resolved color) for
;; the indexed formats, so those are mapped to grayscale here; rgb332,
;; rgb565 and rgb888 are returned already resolved to rgb888 by getpix.
(defun index-levels (fmt)
  (if (eq fmt 'indexed2) 2
      (if (eq fmt 'indexed4) 4
          (if (eq fmt 'indexed16) 16 0))))

(defun save-ppm (img fmt filename)
  (let ((dims (img-dims img))
        (w (car dims))
        (h (car (cdr dims)))
        (levels (index-levels fmt))
        (fd (f-open filename "wb"))
        (buf (array-create (* w h 3))))
    (progn
      (f-write-str fd (str-merge "P6\n" (to-str w) " " (to-str h) "\n255\n"))
      (define pos 0)
      (loopfor y 0 (< y h) (+ y 1)
        (loopfor x 0 (< x w) (+ x 1)
          (progn
            (define p (img-getpix img x y))
            (if (> levels 0)
                (let ((g (/ (* p 255) (- levels 1))))
                  (progn (bufset-u8 buf pos g) (bufset-u8 buf (+ pos 1) g) (bufset-u8 buf (+ pos 2) g)))
                (progn (bufset-u8 buf pos (shr p 16))
                       (bufset-u8 buf (+ pos 1) (bitwise-and (shr p 8) 255))
                       (bufset-u8 buf (+ pos 2) (bitwise-and p 255))))
            (define pos (+ pos 3)))))
      (f-write fd buf)
      (f-close fd))))

;; ---- timing ----
;; thunk is a zero-argument closure performing the one drawing call to
;; be timed. img/fmt/w/h/param are only used for the csv row and the
;; optional ppm dump, not for the timed call itself.
;; Dumping every (shape, format, size) combination is far too slow (a
;; ppm dump is a per-pixel img-getpix loop in interpreted lisp, ~0.75s
;; each at 640x480), and unnecessary -- all three sizes are still
;; benchmarked (see the csv), this is just a quick visual sanity check
;; that each shape draws what it claims to, so one representative
;; (format, size) pair per shape is enough.
(define ppm-format 'rgb888)
(define ppm-size "medium")

(defun bench-case (shape fmt w h param iters img thunk)
  (progn
    (img-clear img)
    (define t0 (systime))
    (loopfor i 0 (< i iters) (+ i 1) (thunk))
    (define dt (secs-since t0))
    (csv-row (list shape (sym2str fmt) (to-str w) (to-str h) param
                   (to-str iters)
                   (str-from-n dt "%.6f")
                   (str-from-n (/ (* dt 1000000) iters) "%.3f")))
    (if (and render-ppm (eq fmt ppm-format) (= (str-cmp param ppm-size) 0))
        (progn
          ;; A fresh single draw for the snapshot, not the buffer state
          ;; left over from `iters` repeated draws above: dotted shapes
          ;; go through line()'s dash-phase counter, a `static int`
          ;; that keeps advancing across all `iters` calls, so without
          ;; this the dump would show the union of `iters` phase-shifted
          ;; dash patterns overlaid -- solid, not dashed.
          (img-clear img)
          (thunk)
          (save-ppm img fmt (str-merge "ppm/" shape "_" (sym2str fmt) "_" param ".ppm")))
        nil)))

;; ---- shape catalog ----
;; Each *-thunk function takes (img cx cy r color) and returns a
;; zero-argument closure performing the one drawing call to be timed.
(defun rect-filled-thunk (img cx cy r color)
  (lambda () (img-rectangle img (- cx r) (- cy r) (* r 2) (* r 2) color '(filled))))

(defun rect-outline-thunk (img cx cy r color)
  (lambda () (img-rectangle img (- cx r) (- cy r) (* r 2) (* r 2) color '(thickness 3))))

(defun triangle-filled-thunk (img cx cy r color)
  (lambda () (img-triangle img (- cx r) (+ cy r) (+ cx r) (+ cy r) cx (- cy r) color '(filled))))

(defun triangle-outline-thunk (img cx cy r color)
  (lambda () (img-triangle img (- cx r) (+ cy r) (+ cx r) (+ cy r) cx (- cy r) color '(thickness 3))))

(defun line-thick-thunk (img cx cy r color)
  (lambda () (img-line img (- cx r) (- cy r) (+ cx r) (+ cy r) color '(thickness 5))))

(defun circle-filled-thunk (img cx cy r color)
  (lambda () (img-circle img cx cy r color '(filled))))

(defun circle-outline-thunk (img cx cy r color)
  (lambda () (img-circle img cx cy r color)))

(defun ring-thunk (img cx cy r color)
  (lambda () (img-circle img cx cy r color '(thickness 5))))

(defun arc-thin-thunk (img cx cy r color)
  (lambda () (img-arc img cx cy r 0 270 color)))

(defun arc-ring-thunk (img cx cy r color)
  (lambda () (img-arc img cx cy r 0 270 color '(thickness 5))))

(defun arc-sector-filled-thunk (img cx cy r color)
  (lambda () (img-circle-sector img cx cy r 0 270 color '(filled))))

(defun arc-segment-filled-thunk (img cx cy r color)
  (lambda () (img-circle-segment img cx cy r 0 270 color '(filled))))

;; ---- narrow-angle variants ----
;; Every arc/sector/segment case above spans 0-270, always the reflex
;; (>180 deg) case -- the union-of-two-halfplanes wedge-clip branch. A
;; narrow 33 deg span exercises the other branch (single-halfplane
;; intersect) instead, which earlier investigation found is where
;; narrow-angle edge-case bugs actually showed up (degenerate cap
;; points, stray-pixel leaks) -- see the arc rasterizer rewrite notes.
(defun arc-thin-narrow-thunk (img cx cy r color)
  (lambda () (img-arc img cx cy r 0 33 color)))

(defun arc-ring-narrow-thunk (img cx cy r color)
  (lambda () (img-arc img cx cy r 0 33 color '(thickness 5))))

(defun arc-sector-filled-narrow-thunk (img cx cy r color)
  (lambda () (img-circle-sector img cx cy r 0 33 color '(filled))))

(defun arc-segment-filled-narrow-thunk (img cx cy r color)
  (lambda () (img-circle-segment img cx cy r 0 33 color '(filled))))

;; ---- dotted / rounded variants ----
(defun line-dotted-thunk (img cx cy r color)
  (lambda () (img-line img (- cx r) (- cy r) (+ cx r) (+ cy r) color '(dotted 4 4))))

(defun rect-outline-dotted-thunk (img cx cy r color)
  (lambda () (img-rectangle img (- cx r) (- cy r) (* r 2) (* r 2) color '(thickness 3) '(dotted 4 4))))

(defun rect-outline-rounded-thunk (img cx cy r color)
  (lambda () (img-rectangle img (- cx r) (- cy r) (* r 2) (* r 2) color '(thickness 3) (list 'rounded (/ r 3)))))

(defun rect-filled-rounded-thunk (img cx cy r color)
  (lambda () (img-rectangle img (- cx r) (- cy r) (* r 2) (* r 2) color '(filled) (list 'rounded (/ r 3)))))

(defun circle-outline-dotted-thunk (img cx cy r color)
  (lambda () (img-circle img cx cy r color '(dotted 4 4))))

(defun arc-ring-dotted-thunk (img cx cy r color)
  (lambda () (img-arc img cx cy r 0 270 color '(thickness 5) '(dotted 4 4))))

(defun arc-ring-rounded-thunk (img cx cy r color)
  (lambda () (img-arc img cx cy r 0 270 color '(thickness 5) '(rounded))))

(defun arc-sector-dotted-thunk (img cx cy r color)
  (lambda () (img-circle-sector img cx cy r 0 270 color '(dotted 4 4))))

(defun arc-segment-dotted-thunk (img cx cy r color)
  (lambda () (img-circle-segment img cx cy r 0 270 color '(dotted 4 4))))

;; ---- blit / text ----
;; The .bin font format used by img-text (5x7 built-in or a loaded
;; bitmap font like this one) is a different thing from the ttf_extensions
;; module's font handling -- that's a separate future-work benchmark.
(define bench-font (load-file (f-open "../../tests/sdl_tests/font_16_26.bin" "r")))

;; blit's copy_pixel goes through getpixel(src)/putpixel(dest), so it's
;; format-agnostic -- the source sprite is always rgb888 regardless of
;; the destination canvas's format under test. Built once per thunk
;; construction (not per iteration), so the timed call is blit alone,
;; not "allocate + draw a sprite + blit".
(defun blit-plain-thunk (img cx cy r color)
  (let ((src (img-buffer 'rgb888 r r)))
    (progn
      (img-circle src (/ r 2) (/ r 2) (/ r 2) color '(filled))
      (lambda () (img-blit img src (- cx (/ r 2)) (- cy (/ r 2)) -1)))))

(defun blit-rotated-thunk (img cx cy r color)
  (let ((src (img-buffer 'rgb888 r r)))
    (progn
      (img-circle src (/ r 2) (/ r 2) (/ r 2) color '(filled))
      (lambda () (img-blit img src (- cx (/ r 2)) (- cy (/ r 2)) -1
                            (list 'rotate (/ r 2) (/ r 2) 45))))))

(defun blit-scaled-thunk (img cx cy r color)
  (let ((src (img-buffer 'rgb888 r r)))
    (progn
      (img-circle src (/ r 2) (/ r 2) (/ r 2) color '(filled))
      (lambda () (img-blit img src (- cx (/ r 2)) (- cy (/ r 2)) -1 '(scale 2.0))))))

(defun text-thunk (img cx cy r color)
  (let ((mag (+ 1 (/ r 60))))
    (lambda () (img-text img (- cx 60) (- cy 13) color 0 bench-font "Bench!" (list 'magnify mag)))))

;; Shapes benchmarked at three sizes (10%, 25%, 40% of the canvas's
;; shorter dimension)
(define sized-shapes
  (list (list "rect-filled" rect-filled-thunk)
        (list "rect-outline" rect-outline-thunk)
        (list "rect-filled-rounded" rect-filled-rounded-thunk)
        (list "rect-outline-rounded" rect-outline-rounded-thunk)
        (list "rect-outline-dotted" rect-outline-dotted-thunk)
        (list "triangle-filled" triangle-filled-thunk)
        (list "triangle-outline" triangle-outline-thunk)
        (list "line-thick" line-thick-thunk)
        (list "line-dotted" line-dotted-thunk)
        (list "circle-filled" circle-filled-thunk)
        (list "circle-outline" circle-outline-thunk)
        (list "circle-outline-dotted" circle-outline-dotted-thunk)
        (list "ring" ring-thunk)
        (list "arc-thin" arc-thin-thunk)
        (list "arc-thin-narrow" arc-thin-narrow-thunk)
        (list "arc-ring" arc-ring-thunk)
        (list "arc-ring-narrow" arc-ring-narrow-thunk)
        (list "arc-ring-rounded" arc-ring-rounded-thunk)
        (list "arc-ring-dotted" arc-ring-dotted-thunk)
        (list "arc-sector-filled" arc-sector-filled-thunk)
        (list "arc-sector-filled-narrow" arc-sector-filled-narrow-thunk)
        (list "arc-sector-dotted" arc-sector-dotted-thunk)
        (list "arc-segment-filled" arc-segment-filled-thunk)
        (list "arc-segment-filled-narrow" arc-segment-filled-narrow-thunk)
        (list "arc-segment-dotted" arc-segment-dotted-thunk)
        (list "blit-plain" blit-plain-thunk)
        (list "blit-rotated" blit-rotated-thunk)
        (list "blit-scaled" blit-scaled-thunk)
        (list "text" text-thunk)))

(define size-names (list "small" "medium" "large"))

(defun run-canvas (w h iters)
  (loopforeach fmt formats
    (progn
      (define img (img-buffer fmt w h))
      (define color (color-of fmt))
      (define cx (/ w 2))
      (define cy (/ h 2))
      (define min-dim (if (< w h) w h))
      (define radii (list (/ min-dim 10) (/ min-dim 4) (/ (* min-dim 2) 5)))
      (loopforeach shape-entry sized-shapes
        (let ((shape (car shape-entry))
              (thunk-fn (car (cdr shape-entry))))
          (loopfor i 0 (< i 3) (+ i 1)
            (let ((size-name (ix size-names i))
                  (r (ix radii i)))
              (bench-case shape fmt w h size-name iters img
                          (thunk-fn img cx cy r color))))))
      (print (str-merge "done: " (to-str w) "x" (to-str h) " " (sym2str fmt))))))

(loopforeach c canvases
  (run-canvas (car c) (car (cdr c)) (car (cdr (cdr c)))))

(f-close csv-fd)
(print (str-merge "wrote " csv-filename))
