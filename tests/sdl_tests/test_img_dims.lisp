(sdl-init)

(define win (sdl-create-window "Display library" 400 200))
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

;; Test img-dims function
(define img400x200 (img-buffer 'indexed2 400 200))
(define img200x100 (img-buffer 'indexed4 200 100))
(define img10x10 (img-buffer 'rgb888 10 10))

;; Test getting dimensions
(define dims1 (img-dims img400x200))
(define dims2 (img-dims img200x100))
(define dims3 (img-dims img10x10))

;; Test with invalid argument - returns (0 0) instead of error
(define dims4 (img-dims "not-an-image"))
(define dims5 (img-dims nil))

;; Verify results
(define r1 (and (eq (car dims1) 400) (eq (car (cdr dims1)) 200)))
(define r2 (and (eq (car dims2) 200) (eq (car (cdr dims2)) 100)))
(define r3 (and (eq (car dims3) 10) (eq (car (cdr dims3)) 10)))
(define r4 (and (eq (car dims4) 0) (eq (car (cdr dims4)) 0)))
(define r5 (and (eq (car dims5) 0) (eq (car (cdr dims5)) 0)))

(if (and r1 r2 r3 r4 r5)
    (print "SUCCESS")
    (print "FAILURE"))