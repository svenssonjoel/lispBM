


(sdl-init)

(define w 500)
(define h 500)

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        nil
        (progn  
          (yield 50000)
          (event-loop w)))))

(defun main ()
  (let ((win (sdl-create-window 500 500))
        (rend (sdl-create-soft-renderer win)))
    (progn
      (spawn 100 event-loop win)
      (sdl-renderer-set-color rend 0 0 0)
      (sdl-clear rend)
      (sdl-renderer-set-color rend 255 255 255)
      (sdl-draw-line rend 0 0 w h)
      (sdl-present rend)
      'done
      )))

(defun clean ()
  (gc))
