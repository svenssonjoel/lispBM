


(sdl-init)


(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        nil
        (progn  
          (yield 50000)
          (event-loop w)))))

(defun main ()
    (let ((win (sdl-create-window 500 500)))
      (event-loop win)))
