
;; grid of rooms
;; extract room x/y by (ix (ix map-of-rooms x) y)  
(define map-of-rooms
    '(("snake_room.lisp")))

(define game-state '((room-cid . -1)))

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
        (setq game-state (setassoc game-state 'room-cid room-cid))
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

(define game-running t)

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event) {
        (var room-cid (assoc game-state 'room-cid))
        (if (> room-cid 0) {
            (print "killing the room thread")
            (kill room-cid 0)
            (wait room-cid)
            })
        (custom-destruct w)
        (setq game-running nil)
        }
        (progn  
          (yield 5000)
          (event-loop w)))))


(print "init SDL")
(sdl-init)
(print "creating window")
(define win (sdl-create-window "GAME" 400 400))
(print "window opened")
(define rend (sdl-create-soft-renderer win))
(print "renderer created")

(sdl-renderer-set-color rend 0 0 0)
(sdl-clear rend)
(sdl-renderer-set-color rend 255 255 255)
(sdl-set-active-renderer rend) ;; Connect the renderer to the display library

(define font-file (fopen "Ubuntu-Regular.ttf" "r"))
(define font (load-file font-file))
(define ttf (ttf-prepare font 32 'indexed4 "abcdefghijklmnopqrstuvxyz1234567890+-*/"))
(define aa-green '(0 17408 39168 65280))
(define disp (img-buffer 'rgb332 400 400))

(spawn 100 event-loop win)

(setq game-state (acons 'disp disp game-state))
                                        ;(print game-state)

;; TODO: put everything needed by the room code into game-state.

;; testing and example
(load-room '(0 . 0))
