(sdl-init)

(define win (sdl-create-window "Display library - overlapping alpha circles" 300 300))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

(define img (img-buffer 'rgb888 300 300))
(img-clear img 0x101018)

;; Four overlapping thick rings plus one translucent ring through the middle.
;; This exercises the alpha-compositing ring code with genuine overlap, and
;; a plain thin outline circle (below) exercises the alpha-compositing
;; outline code specifically at its 4 pole pixels.
(define red    (img-color 'regular 0xE04030 160))
(define green  (img-color 'regular 0x30C060 160))
(define blue   (img-color 'regular 0x3080E0 160))
(define yellow (img-color 'regular 0xE0C030 160))
(define white  (img-color 'regular 0xFFFFFF 120))

(define c1 (img-circle img 110 110 70 red    '(thickness 24)))
(define c2 (img-circle img 170 110 70 green  '(thickness 24)))
(define c3 (img-circle img 110 170 70 blue   '(thickness 24)))
(define c4 (img-circle img 170 170 70 yellow '(thickness 24)))
(define c5 (img-circle img 140 140 30 white  '(thickness 30)))

;; A thin (outline, no thickness attribute) alpha circle. Its 4 pole pixels
;; ((cx,cy+-r) and (cx+-r,cy)) must be blended exactly once, not twice --
;; regression test for a bug where the octant mirror's x0==0 step collapsed
;; onto the same 4 pixels via two different putpixel calls each, which is
;; invisible for opaque draws but visibly wrong (over-blended) with alpha.
(define pole-color (img-color 'regular 0xFFFFFF 128))
(define c6 (img-circle img 250 250 20 pole-color))

(define pole-top   (img-getpix img 250 270))
(define pole-bot   (img-getpix img 250 230))
(define pole-right (img-getpix img 270 250))
(define pole-left  (img-getpix img 230 250))

;; expected: single blend of white(255) at alpha=128 over the 0x101018
;; background, matching alpha_blend_rgb888's div255(src*a + dst*(255-a) + 127)
(defun blend1 (src dst alpha) (/ (+ (* src alpha) (+ (* dst (- 255 alpha)) 127)) 255))
(define expected-r (blend1 0xFF 0x10 128))
(define expected-g (blend1 0xFF 0x10 128))
(define expected-b (blend1 0xFF 0x18 128))
(define expected-pole (+ (shl expected-r 16) (+ (shl expected-g 8) expected-b)))

(define poles-ok (and (= pole-top expected-pole)
                       (= pole-bot expected-pole)
                       (= pole-right expected-pole)
                       (= pole-left expected-pole)))

;; overlap sanity: where red and green rings cross should differ from
;; either ring's own (single-blend) color -- proof genuine compositing
;; happened rather than a flat overwrite.
(define overlap-px (img-getpix img 140 110))
(define red-only-px (img-getpix img 85 110))
(define overlap-differs (and (not (= overlap-px red-only-px))
                              (not (= overlap-px 0x101018))))

(define dims (img-dims img))
(define is_buffer (img-buffer? img))

(disp-render img 0 0)
(save-img img "sdl_tests/png_out/test_img_circle_overlap_alpha.png")

(if (and c1 c2 c3 c4 c5 c6
         is_buffer (eq dims '(300 300))
         poles-ok
         overlap-differs)
    (print "SUCCESS")
    (print "FAILURE"))
