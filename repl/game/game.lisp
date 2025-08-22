


(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))


;; grid of rooms
;; extract room x/y by (ix (ix map-of-rooms x) y)  
(define map-of-rooms
    '(("test_room.lisp")))

(define game-state '())

;; Load and execute a room. after a room function completes to execute
;; it can garbage collected. 
(define load-room
    (lambda (pos)
      (let (((x . y) pos)
            (room-file (ix (ix map-of-rooms x) y))
            (room-h (fopen room-file "r"))
            (room (load-file room-h))
            (room-fun (read-eval-program room))
            (room-cid (spawn room-fun game-state)))
        {
        (fclose room-h)
        room-cid
        })))

;; Load the tile system
(define load-tile-system (lambda ()
  {
    (var tile-file (fopen "room_tiles.lisp" "r"))
    (var tile-code (load-file tile-file))
    (fclose tile-file)
    (read-eval-program tile-code)
  }))

(load-tile-system)

(defun main () {
       (define win (sdl-create-window "GAME" 400 400))
       (define rend (sdl-create-soft-renderer win))
       (spawn 100 event-loop win)
       (sdl-renderer-set-color rend 0 0 0)
       (sdl-clear rend)
       (sdl-renderer-set-color rend 255 255 255)
       (sdl-set-active-renderer rend) ;; Connect the renderer to the display library
       
       (define font-file (fopen "Ubuntu-Regular.ttf" "r"))
       (define font (load-file font-file))
       (define ttf (ttf-prepare font 32 'indexed4 "abcdefghijklmnopqrstuvxyz1234567890+-*/"))
       (define aa-green '(0 17408 39168 65280))
       (define disp (img-buffer 'rgb332 400 400))
       ;;(ttf-text disp 10 80 aa-green ttf "ttf-font")

       (setq game-state (acons 'disp disp game-state))
       ;(print game-state)
       
       ;; TODO: put everything needed by the room code into game-state.
       
       ;; testing and example
       (print (load-room '(0 . 0)))

       
       ;;(disp-render disp 0 0 (list ))
       })

(main)
